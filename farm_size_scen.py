# Usage: python -m farm_size_scen

# Farm-size data processed in GEE. 
# Code for area by farm-size: https://code.earthengine.google.com/c46eb857d355e0f76da1d34ce39fbd84
# Code for production by farm_size: https://code.earthengine.google.com/9ca0fabc46a050add5fffca6e9360af1

import pandas as pd
import numpy as np
import math
import itertools

def get_sua_data(regions):
    sua = pd.read_csv('../../DPhil/data/FAOSTAT_A-S_E/SUA_Crops_Livestock_E_All_Data_(Normalized)/SUA_Crops_Livestock_E_All_Data_(Normalized).csv',
                      encoding='latin1', low_memory=False)
    sua = sua[(sua['Element'].isin(['Feed', 'Food supply quantity (tonnes)', 'Loss', 'Other uses (non-food)',
                                    'Residuals', 'Seed', 'Processed', 'Tourist consumption']))
        & (sua['Year'].isin([2018, 2019, 2020, 2021, 2022]))].groupby(['Area', 'Area Code (M49)', 'Element', 'Item'])[['Value']].mean().reset_index().pivot(
        index=['Area', 'Area Code (M49)', 'Item'], columns='Element', values='Value').reset_index()
    
    sua['Area Code (M49)'] = sua.apply(lambda row: int(row['Area Code (M49)'][1:]), axis=1)
    sua = sua.rename(columns={'Area Code (M49)': 'M49 Code'}).reset_index(drop=True)
    sua = sua.merge(regions[['M49 Code', 'iso3']].drop_duplicates())
    
    sua = sua.drop(['Area', 'M49 Code'], axis=1)
    sua = sua.rename(columns={'Food supply quantity (tonnes)': 'Food', 'Loss': 'Losses', 'Processed': 'Processing'})
    sua = sua.fillna(0)
    sua['Domestic supply quantity'] = sua['Feed'] + sua['Food'] + sua['Losses'] + sua['Seed'] + sua['Processing'] + sua['Other uses (non-food)'] \
    + sua['Tourist consumption'] + sua['Residuals']
    sua.loc[sua['Domestic supply quantity']<0, 'Domestic supply quantity'] = 0
    
    return sua


#### need to split oils and nuts etc.
## did the same thing for faostat data as well, there I split olives and coconuts too, but that is not possible here 
# currently the values in other nuts and other oils are duplicated
def split_prod_area(df, sua):

    def _split(df_crop, df_sua, other_crop):
        df_sua.loc[df_sua['Processing']<0, 'Processing'] = 0
        df_sua = df_sua.fillna(0)
        df_sua['proc_prop'] = df_sua['Processing'] / df_sua['Domestic supply quantity']
        df_sua.loc[df_sua['Domestic supply quantity']==0, 'proc_prop'] = 0
        df_sua.loc[df_sua['proc_prop']>1, 'proc_prop'] = 1
    
        df_crop = df_crop.merge(df_sua[['iso3', 'proc_prop']], how='left').fillna(0)
        
        df_crop1 = df_crop.copy()
        df_crop1['production_tons'] = df_crop1['production_tons'] * (1-df_crop1['proc_prop'])
        df_crop1['area_ha'] = df_crop1['area_ha'] * (1-df_crop1['proc_prop'])
        df_crop1 = df_crop1.drop('proc_prop', axis=1)
        
        df_crop2 = df_crop.copy()
        df_crop2['crop'] = other_crop
        df_crop2['production_tons'] = df_crop2['production_tons'] * df_crop2['proc_prop']
        df_crop2['area_ha'] = df_crop2['area_ha'] * df_crop2['proc_prop']
        df_crop2 = df_crop2.drop('proc_prop', axis=1)
        return df_crop1, df_crop2
    
    # groundnuts : between groundnuts and other oils
    df_groundnuts = df[df['crop']=='groundnuts']    
    
    df_sua1 = sua[sua['Item']=='Groundnuts, excluding shelled']
    df_sua2 = sua[sua['Item'].isin(['Groundnuts, shelled', 'Prepared groundnuts'])]
    df_sua2 = df_sua2.groupby('iso3')[['Domestic supply quantity']].sum().reset_index()
    df_sua = df_sua1[['iso3', 'Processing']].merge(df_sua2[['iso3', 'Domestic supply quantity']], how='left')
    df_sua.loc[df_sua['Domestic supply quantity'].isna(), 'Domestic supply quantity'] = 0  
    df_sua['Processing'] = df_sua['Processing'] - df_sua['Domestic supply quantity']
    df_sua = df_sua[['iso3', 'Processing']].merge(df_sua1[['iso3', 'Domestic supply quantity']], how='left')

    df_groundnuts1, df_groundnuts2 = _split(df_groundnuts, df_sua, 'other_veg_oils')
    
    # sunflower seeds: between sunflower oil and other nuts
    df_sunflower = df[df['crop']=='sunflower']
    df_sunflower['crop'] = 'other_nuts_seeds'
    df_sua = sua[sua['Item']=='Sunflower seed'][['iso3', 'Processing', 'Domestic supply quantity']]
    df_sunflower1, df_sunflower2 = _split(df_sunflower, df_sua, 'sunflower')

    # sesame seeds: between other nuts and other oils
    df_sesame = df[df['crop']=='sesame_seed']
    df_sesame['crop'] = 'other_nuts_seeds'
    df_sua = sua[sua['Item']=='Sesame seed'][['iso3', 'Processing', 'Domestic supply quantity']]
    df_sesame1, df_sesame2 = _split(df_sesame, df_sua, 'other_veg_oils')

    # coconut: between other oils and tropical fruits
    df_coconut = df[df['crop']=='coconut']
    df_coconut['crop'] = 'tropical_fruits'
    
    df_sua1 = sua[sua['Item']=='Coconuts, in shell']
    df_sua2 = sua[sua['Item']=='Coconuts, desiccated']
    df_sua = df_sua1[['iso3', 'Processing']].merge(df_sua2[['iso3', 'Domestic supply quantity']], how='left')
    df_sua.loc[df_sua['Domestic supply quantity'].isna(), 'Domestic supply quantity'] = 0
    df_sua['Processing'] = df_sua['Processing'] - df_sua['Domestic supply quantity']
    df_sua = df_sua[['iso3', 'Processing']].merge(df_sua1[['iso3', 'Domestic supply quantity']], how='left')
    
    df_coconut1, df_coconut2 = _split(df_coconut, df_sua, 'other_veg_oils')

    # temprate: olives between other oils and temperate fruits
    df_temperate = df[df['crop']=='temperate_fruits']
    
    df_sua1 = sua[sua['Item']=='Olives']
    df_sua2 = sua[sua['Item']=='Olives preserved']
    df_sua = df_sua1[['iso3', 'Processing']].merge(df_sua2[['iso3', 'Domestic supply quantity']], how='left')
    df_sua.loc[df_sua['Domestic supply quantity'].isna(), 'Domestic supply quantity'] = 0
    df_sua['Processing'] = df_sua['Processing'] - df_sua['Domestic supply quantity']
    df_sua = df_sua[['iso3', 'Processing']].merge(df_sua1[['iso3', 'Item', 'Domestic supply quantity']], how='left')
    df_sua3 = sua[sua['Item'].isin(['Apples', 'Grapes', 'Blueberries', 'Cherries', 'Cranberries', 'Currants', 'Gooseberries', 
                                   'Other berries and fruits of the genus vaccinium n.e.c.', 'Other pome fruits', 'Other stone fruits', 
                                   'Peaches and nectarines', 'Pears', 'Persimmons', 'Plums and sloes', 'Quinces', 'Raspberries', 
                                   'Sour cherries', 'Strawberries'])][['iso3', 'Item', 'Processing', 'Domestic supply quantity']]
    df_sua3['Processing'] = 0
    df_sua = pd.concat([df_sua, df_sua3], axis=0, ignore_index=True)
    df_sua = df_sua.groupby(['iso3'])[['Processing', 'Domestic supply quantity']].sum().reset_index()
    df_temperate1, df_temperate2 = _split(df_temperate, df_sua, 'other_veg_oils')

    # other nuts: between other nuts and other oils
    df_other_nuts_seeds = df[df['crop']=='other_nuts_seeds']
    df_sua = sua[sua['Item'].isin(['Linseed', 'Hempseed', 'Safflower seed', # both oils and nuts_seeds
                                   'Poppy seed', # only nuts_seeds
                                   'Castor oil seeds', 'Cotton seed', 'Mustard seed' # only oils
                                  ])][['iso3', 'Item', 'Processing', 'Domestic supply quantity']]
    df_sua.loc[df_sua['Item'].isin(['Poppy seed']), 'Processing'] = 0
    df_sua.loc[df_sua['Item'].isin(['Castor oil seeds', 'Cotton seed', 'Mustard seed']), 'Processing'] = df_sua.loc[df_sua['Item'].isin(
        ['Castor oil seeds', 'Cotton seed', 'Mustard seed'])][ 'Domestic supply quantity']
    df_sua = df_sua.groupby(['iso3'])[['Processing', 'Domestic supply quantity']].sum().reset_index()
    df_other_nuts_seeds1, df_other_nuts_seeds2 = _split(df_other_nuts_seeds, df_sua, 'other_veg_oils')

    # combine all
    df = df[~df['crop'].isin(['groundnuts', 'sunflower', 
                              'sesame_seed', 'coconut', 
                              'temperate_fruits',
                              'other_nuts_seeds'])]
    # rest_of_crops for nuts and seeds as well
    # 'Almonds, in shell', 'Brazil nuts, in shell', 'Cashew nuts, in shell', 'Chestnuts, in shell', 'Hazelnuts, in shell', 
    # 'Other nuts (excluding wild edible nuts and groundnuts), in shell, n.e.c.', 'Pistachios, in shell', 'Walnuts, in shell',
    df.loc[df['crop']=='rest_of_crops', 'crop'] = 'other_nuts_seeds' 
    df = pd.concat([df, 
                    df_groundnuts1, df_groundnuts2, 
                    df_sunflower1, df_sunflower2, 
                    df_sesame1, df_sesame2,
                    df_coconut1, df_coconut2,
                    df_temperate1, df_temperate2,
                    df_other_nuts_seeds1, df_other_nuts_seeds2], axis=0, ignore_index=True)
    df = df.groupby(['iso3', 'crop', 'farm_size_group']).sum().reset_index()
    return df
    

def get_farm_size_data(regions, sua):

    def _calc_props(g):
        if g['production_tons'].sum()>0:
            g['prop'] = g['production_tons'] / g['production_tons'].sum()
        else:
            g['prop'] = 1/len(g)
        return g

    def _add_all_farm_size_groups(g):

        abb = g['abbreviation'].values[0]
        country = g['Region or country'].values[0]
        crop = g['IMPACT_code'].values[0]
        
        d = pd.DataFrame({"farm_size_group": [1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 5000]})
        g = g.merge(d, how='outer')
        g['abbreviation'] = abb
        g['Region or country'] = country
        g['IMPACT_code'] = crop
        g['yield_t_ha'] = g['yield_t_ha'].interpolate(limit_direction='both')
        g = g.fillna(0)
        return g
        
    
    farm_size_prod = pd.read_csv('../../data/farm_size/farm_size_groups/all_crops_prod_by_farmsize_country_gadm_finalV2.csv')
    farm_size_prod.loc[farm_size_prod['COUNTRY']=='India', 'GID_0'] = 'IND'
    farm_size_prod.loc[farm_size_prod['COUNTRY']=='Pakistan', 'GID_0'] = 'PAK'
    farm_size_prod.loc[farm_size_prod['COUNTRY']=='China', 'GID_0'] = 'CHN'

    farm_size_prod = farm_size_prod.rename(columns={'GID_0': 'iso3'}).groupby(['iso3', 'crop', 'farm_size_group'])[['production_tons']].sum().reset_index()

    farm_size_area = pd.read_csv('../../data/farm_size/farm_size_groups/all_crops_area_by_farmsize_country_gadm_finalV2.csv')
    farm_size_area.loc[farm_size_area['COUNTRY']=='India', 'GID_0'] = 'IND'
    farm_size_area.loc[farm_size_area['COUNTRY']=='Pakistan', 'GID_0'] = 'PAK'
    farm_size_area.loc[farm_size_area['COUNTRY']=='China', 'GID_0'] = 'CHN'

    farm_size_area = farm_size_area.rename(columns={'GID_0': 'iso3'}).groupby(['iso3', 'crop', 'farm_size_group'])[['area_ha']].sum().reset_index()

    # merge production and area 
    farm_size_prod = farm_size_prod.merge(farm_size_area)
    farm_size_prod.loc[farm_size_prod['area_ha']==0, 'production_tons'] = 0
    farm_size_prod.loc[farm_size_prod['production_tons']==0, 'area_ha'] = 0

    # split production and area for overlapting categories 
    farm_size_prod = split_prod_area(farm_size_prod, sua)
    
    crop_dict = {'wheat': 'jwhea', 
                 'rice': 'jrice', 
                 'maize': 'jmaiz', 
                 'barley': 'jbarl', 
                 'millet': 'jmill', 
                 'sorghum': 'jsorg',
                 'other_cereals': 'jocer', 
                 'cassava': 'jcass', 
                 'potato': 'jpota', 
                 'yams': 'jyams', 
                 'sweet_potato': 'jswpt',
                 'other_roots': 'jorat', 
                 'banana': 'jbana', 
                 'plantain': 'jplnt', 
                 'tropical_fruits': 'jsubf',
                 'temperate_fruits': 'jtemf', 
                 'vegetables': 'jvege', 
                 'beans': 'jbean', 
                 'chickpeas': 'jchkp', 
                 'lentils': 'jlent',
                 'pigeon_peas': 'jpigp', 
                 'cowpeas': 'jcowp', 
                 'other_legumes': 'jopul', 
                 'soybean': 'jsoyb', 
                 'groundnuts': 'jgrnd',
                 'other_nuts_seeds': 'jothr', 
                 'rapeseed': 'jrpsd', 
                 'sunflower': 'jsnfl', 
                 'other_veg_oils': 'jtols',
                 'palm_oil': 'jpalm', 
                 'sugar_cane': 'jsugc', 
                 'sugar_beet': 'jsugb'}
    
    farm_size_prod['IMPACT_code'] = farm_size_prod['crop'].map(crop_dict)
    farm_size_prod = farm_size_prod.merge(regions[['abbreviation', 'Region or country', 'iso3']])
    farm_size_prod = farm_size_prod.groupby(['abbreviation', 'Region or country', 'IMPACT_code',
                                             'farm_size_group'])[['production_tons', 'area_ha']].sum().reset_index()

    # calculate yields by farm size group
    farm_size_prod['yield_t_ha'] = farm_size_prod['production_tons'] / farm_size_prod['area_ha']
    
    farm_size_prod = farm_size_prod.groupby(['abbreviation', 'Region or country', 'IMPACT_code']).apply(
        lambda g: _calc_props(g)).reset_index(drop=True).drop(['production_tons', 'area_ha'], axis=1)
    
    farm_size_prod = farm_size_prod.groupby(['abbreviation', 'IMPACT_code']).apply(lambda g: _add_all_farm_size_groups(g)).reset_index(drop=True) # add all farm size groups and fill nulls
    farm_size_prod['year'] = 2020
    
    farm_size_prod['prop_consolidation'] = farm_size_prod['prop']
    farm_size_prod['prop_fragmentation'] = farm_size_prod['prop']

    return farm_size_prod

#### x% from each bucket moves to the next - could apply a differnt logic or higher intensity
def farm_size_vary(df, year, factor=0.1):

    def _shift_production(g):

        g["consolidation"] = g["prop_consolidation"] * factor
        g["fragmentation"] = g["prop_fragmentation"] * factor
        
        g["prop_consolidation"] = g["prop_consolidation"] + g["consolidation"].shift(periods=1).fillna(0) 
        g.loc[g.index[-1], 'consolidation'] = 0
        g["prop_consolidation"] = g["prop_consolidation"] - g["consolidation"]
        
        g["prop_fragmentation"] = g["prop_fragmentation"] + g["fragmentation"].shift(periods=-1).fillna(0) 
        g.loc[g.index[0], 'fragmentation'] = 0
        g["prop_fragmentation"] = g["prop_fragmentation"] - g["fragmentation"]
        g = g.drop(['consolidation', 'fragmentation'], axis=1)

        return g

    df = df.copy()
    df['year'] = year
    df = df.groupby(['abbreviation', 'IMPACT_code']).apply(lambda g: _shift_production(g)).reset_index(drop=True)

    return df


def get_trend_direction(regions):
    
    def _get_trend(g):
        
        year_min = g['Year'].min()
        year_max = g['Year'].max()
    
        if len(g[g['Year']==2020])>0 and year_max>2020:
            year_min = 2020
        if len(g[g['Year']==2050])>0:
            year_max = 2050
                
        first = g.loc[g['Year']==year_min]['Median'].values[0]
        last = g.loc[g['Year']==year_max]['Median'].values[0]
    
        if first<last:
            return 'consolidation'
        if first>last:
            return 'fragmentation'
        return 'no change'

    trends = pd.read_csv('../../data/farm_size/trends/farm size_ssp2_country.csv')
    trends = trends.groupby('ISO3_CODE').apply(lambda g: _get_trend(g)).reset_index().rename(columns={'ISO3_CODE': 'iso3', 0: 'direction'})
    trends = trends.merge(regions, how='right')
    
    # take care of abbreviations with multiple iso3 values and they either have nulls or dont match
    trends.loc[trends['abbreviation']=='BLX', 'direction'] = 'consolidation'
    trends.loc[trends['abbreviation']=='CHM', 'direction'] = 'consolidation'
    trends.loc[trends['abbreviation']=='CRB', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='DNK', 'direction'] = 'consolidation'
    trends.loc[trends['abbreviation']=='GSA', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='ITP', 'direction'] = 'consolidation'
    trends.loc[trends['abbreviation']=='OAO', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='OBN', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='OIO', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='OPO', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='OSA', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='RAP', 'direction'] = 'consolidation' 
    
    # repace nulls based on surrounding countries 
    trends.loc[trends['abbreviation']=='DJI', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='GNQ', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='BLZ', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='PRK', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='BTN', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='ISL', 'direction'] = 'consolidation' 
    trends.loc[trends['abbreviation']=='FJI', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='SLB', 'direction'] = 'fragmentation' 
    trends.loc[trends['abbreviation']=='VUT', 'direction'] = 'fragmentation' 
    
    trends = trends[['abbreviation', 'direction']].drop_duplicates().reset_index(drop=True)
    return trends

if __name__ == '__main__':

    regions = pd.read_csv('../../data/farm_size/herrero_impact/regions2.csv').rename(columns={'Abbreviation': 'abbreviation'})
    sua = get_sua_data(regions)
    farm_size_prod = get_farm_size_data(regions, sua) 

    df_list = []
    df_list.append(farm_size_prod)
    
    for year in [2025, 2030, 2035, 2040, 2045, 2050]:
        farm_size_prod = farm_size_vary(farm_size_prod, year, factor=0.05) # changed this to 5% for 5 year intervals
        df_list.append(farm_size_prod)

    df = pd.concat(df_list, axis=0, ignore_index=True)

    farm_size_dict = {
        1: 0.5,
        2: 1.5,
        5: 3.5,
        10: 7.5,
        20: 15,
        50: 35,
        100: 75,
        200: 150,
        500: 350,
        1000: 750,
        5000: 3000,
    }
    df['farm_size_group_mean'] = df['farm_size_group'].map(farm_size_dict)
    df.to_csv('../../data/farm_size/farm_size_groups/farm_size_trends.csv', index=False)

    trends = get_trend_direction(regions)
    trends.to_csv('../../data/farm_size/farm_size_groups/trend_direction.csv', index=False)



    