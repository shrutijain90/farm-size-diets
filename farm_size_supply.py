# Usage: python -m farm_size_supply

import pandas as pd
import numpy as np
import math
import itertools

def get_scenario_outputs(scen_list, crop_codes):
    
    data_dir = '../../DPhil/OPSIS/Data/Trade_clearance_model/Output/Trade_allocation_future/'
    
    country_output_all = []
    trade_output_all = []
    
    for crop_code in crop_codes:
        for scen in scen_list:
            if crop_code in ['jgrnd', 'jothr', 'jrpsd', 'jsnfl', 'jtols', 'jpalm', 'jsugc', 'jsugb'] and scen[0] not in ['BMK', 'FLX']:
                country_output = pd.read_csv(f'{data_dir}Country_output/country_output_SSP2_FLX_2500kcal_{scen[1]}_{scen[2]}_{crop_code}.csv')
                trade_output = pd.read_csv(f'{data_dir}Trade_output/trade_output_SSP2_FLX_2500kcal_{scen[1]}_{scen[2]}_{crop_code}.csv')
                country_output['diet_scn'] = scen[0]
                trade_output['diet_scn'] = scen[0]
            else:
                country_output = pd.read_csv(f'{data_dir}Country_output/country_output_SSP2_{scen[0]}_2500kcal_{scen[1]}_{scen[2]}_{crop_code}.csv')
                trade_output = pd.read_csv(f'{data_dir}Trade_output/trade_output_SSP2_{scen[0]}_2500kcal_{scen[1]}_{scen[2]}_{crop_code}.csv')
            country_output_all.append(country_output)
            trade_output_all.append(trade_output)
    
    country_output_all = pd.concat(country_output_all, axis=0, ignore_index=True)
    trade_output_all = pd.concat(trade_output_all, axis=0, ignore_index=True)

    return country_output_all, trade_output_all
    

def calc_num_farms(df, crop_code):
    
    data_dir = '../../DPhil/OPSIS/Data/Trade_clearance_model/Input/'
    
    yield_2020 = pd.read_csv(f'{data_dir}Country_data/country_information_{crop_code}.csv')[['abbreviation', 'yield_t_ha']]
    yield_scn = pd.read_csv(f'{data_dir}Future_scenarios/SSP2/supply_scn/IMPACT_future_supply_{crop_code}.csv')
    yield_df = yield_2020.merge(yield_scn)
    yield_df['yield_t_ha'] = yield_df['yield_t_ha'] * yield_df['scaling_factor_yield']
    yield_df = yield_df[['abbreviation', 'IMPACT_code', 'year', 'RCP', 'yield_t_ha']]
    yield_df = yield_df[yield_df['RCP'].isin(['2.6', '7.0'])]
    yield_df['RCP'] = yield_df['RCP'].astype(float)

    df = df.fillna(0)
    df.loc[(df['supply']!=0) & (df['yield_t_ha']==0), 'yield_t_ha'] = df['yield_t_ha'].mean()
    df['area_ha'] = df['supply'] / df['yield_t_ha']
    df = df.fillna(0)
    df_grouped = df.groupby(['abbreviation', 'year', 'IMPACT_code', 
                             'diet_scn', 'RCP', 'lib_scn'])[['supply', 'area_ha']].sum().reset_index()
    df_grouped['yield_t_ha'] = df_grouped['supply'] / df_grouped['area_ha']
    df_grouped = df_grouped.fillna(0)

    df_grouped = df_grouped.merge(yield_df.rename(columns={'yield_t_ha': 'yield_t_ha_expected'}))
    df_grouped.loc[(df_grouped['yield_t_ha']==0), 'yield_t_ha'] = df_grouped['yield_t_ha'].mean()
    df_grouped['yield_factor'] = df_grouped['yield_t_ha_expected'] / df_grouped['yield_t_ha']

    df = df.merge(df_grouped[['abbreviation', 'year', 'IMPACT_code', 
                              'diet_scn', 'RCP', 'lib_scn', 'yield_factor']])
    df['yield_t_ha'] = df['yield_t_ha'] * df['yield_factor']
    df = df.drop('yield_factor', axis=1)
    df['area_ha'] = df['supply'] / df['yield_t_ha']
    df = df.fillna(0)

    df['num_farms'] = df['area_ha'] / df['farm_size_group_mean']
    df['num_farms_min'] = df['area_ha'] / df['farm_size_group']

    return df

# or something else? maybe random selection from different intensities on each side?
def select_size_scn(farm_size_prod_all, trends, size_scen):
    if size_scen=='same':
        farm_size_prod = farm_size_prod_all.drop(['prop_consolidation', 'prop_fragmentation'], axis=1)
    if size_scen=='consolidate':
        farm_size_prod = farm_size_prod_all.drop(['prop', 'prop_fragmentation'], axis=1).rename(columns={'prop_consolidation': 'prop'})
    if size_scen=='fragment':
        farm_size_prod = farm_size_prod_all.drop(['prop', 'prop_consolidation'], axis=1).rename(columns={'prop_fragmentation': 'prop'})
    if size_scen=='variable':
        farm_size_prod = farm_size_prod_all.merge(trends)
        farm_size_prod.loc[farm_size_prod['direction']=='consolidation', 'prop'] = farm_size_prod.loc[farm_size_prod['direction']=='consolidation']['prop_consolidation']
        farm_size_prod.loc[farm_size_prod['direction']=='fragmentation', 'prop'] = farm_size_prod.loc[farm_size_prod['direction']=='fragmentation']['prop_fragmentation']
        farm_size_prod = farm_size_prod.drop(['prop_consolidation', 'prop_fragmentation', 'direction'], axis=1)

    return farm_size_prod

    

if __name__ == '__main__':

    regions = pd.read_csv('../../data/farm_size/herrero_impact/regions2.csv').rename(columns={'Abbreviation': 'abbreviation'})

    ### Scenarios ###
    scen_diet = ['BMK', 'FLX', 'PSC', 'VEG', 'VGN']  
    scen_clim = ['7'] #2.6
    scen_lib = ['low'] #'high'
    scen_list = list(itertools.product(*[scen_diet, scen_clim, scen_lib]))
    crop_codes = ['jwhea', 'jrice',  'jmaiz', 'jbarl', 'jmill', 'jsorg', 'jocer', 
                  'jcass', 'jpota', 'jyams', 'jswpt', 'jorat', 
                  'jvege', 
                  'jbana', 'jplnt', 'jsubf', 'jtemf', 
                  'jbean', 'jchkp', 'jcowp', 'jlent', 'jpigp', 'jopul',
                  'jsoyb', 
                  'jgrnd', 'jothr',
                  'jrpsd', 'jsnfl', 'jtols',
                  'jpalm',
                  'jsugc', 'jsugb'
                 ]
    
    country_output_all, trade_output_all = get_scenario_outputs(scen_list, crop_codes)
    farm_size_prod_all = pd.read_csv('../../data/farm_size/farm_size_groups/farm_size_trends.csv')
    trends = pd.read_csv('../../data/farm_size/farm_size_groups/trend_direction.csv') 
     
    for size_scen in ['same', 'variable']: #'consolidate', 'fragment', 
        farm_size_prod = select_size_scn(farm_size_prod_all, trends, size_scen)        
        for crop_code in crop_codes:
            print(crop_code)
    
            df = country_output_all[(country_output_all['IMPACT_code']==crop_code)
                ].copy()
            df = df.merge(farm_size_prod)
            df['supply'] = df['supply'] * df['prop']
            
            df = df[['abbreviation', 'year', 'IMPACT_code', 'diet_scn',
                'RCP', 'lib_scn', 'farm_size_group', 'farm_size_group_mean',
                'supply', 'yield_t_ha']]
            df['supply'] = df['supply'] * 1000 # to convert from 1000 tons to tons
    
            # estimate number of farms
            df = calc_num_farms(df, crop_code)
            df['size_scn'] = size_scen
                
            df.to_parquet(f'../../data/farm_size/outputs/supply_by_farm_size_{crop_code}_{size_scen}.parquet.gzip', 
                                               index=False, compression='gzip')
    
            
            df = trade_output_all[(trade_output_all['IMPACT_code']==crop_code)
                ].copy()
            df = df.merge(farm_size_prod.rename(columns={'abbreviation': 'from_abbreviation'}))
            # as some regions are missing in mapspam/farm size data
            df = df[df['to_abbreviation'].isin(df['from_abbreviation'].unique())] 
            df['trade'] = df['trade'] * df['prop']
            df['trade'] = df['trade'] * 1000 # to convert from 1000 tons to tons
            df['size_scn'] = size_scen
            
            df.to_parquet(f'../../data/farm_size/outputs/trade_by_farm_size_{crop_code}_{size_scen}.parquet.gzip', index=False, compression='gzip')

    