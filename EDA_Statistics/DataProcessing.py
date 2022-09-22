########## PSYC 607 -- Project check ##########
import pandas as pd
import numpy as np

# Base table is poblacion, we join the rest to it. 
poblacion_df = pd.read_csv('poblacion.csv')
viviendas_df = pd.read_csv('viviendas.csv')
#hogares_df = pd.read_csv('hogares.csv')
gastospersona_df = pd.read_csv('gastospersona.csv')
ingresos_df = pd.read_csv('ingresos.csv')

# --- Poblacion ---
# we drop unused columns
# poblacion_df.columns
poblacion_df = poblacion_df.drop(columns=['parentesco','madre_hog', 'padre_hog', 'trabajo_mp'])

# we create the unique id of person, household, numren
poblacion_df["id_persona"] = poblacion_df.folioviv.map(str) + poblacion_df.foliohog.map(str) + poblacion_df.numren.map(str)

# drop columns we dont need
poblacion_df = poblacion_df.drop(columns=
[
'disc1', 'disc2', 'disc3','disc4','disc5','disc6','disc7',
'segvol_6', 'segvol_7', 'segvol_1', 'segvol_2', 'segvol_3', 'segvol_4',
'segvol_5', 'forma_c','gradoaprob','antec_esc','hijos_mue', 'hijos_sob',
'motivo_aus', 'act_pnea1', 'act_pnea2', 'num_trabaj',
'causa1','causa2','causa3','causa4','causa5','causa6','causa7',
'segsoc','ss_aa','ss_mm','tipoesc','tiene_b','otorg_b','forma_b',
'tiene_c','otorg_c','razon_1','razon_2','razon_3','razon_4','razon_5',
'razon_6','razon_7','razon_8','razon_9','razon_10','razon_11',
'norecib_1','norecib_2','norecib_3','norecib_4','norecib_5','norecib_6',
'norecib_7','norecib_8','norecib_9','norecib_10','norecib_11',
'noatenc_1','noatenc_2','noatenc_3','noatenc_4','noatenc_5','noatenc_6',
'noatenc_7','noatenc_8','noatenc_9','noatenc_10','noatenc_11','noatenc_12',
'noatenc_13','noatenc_14','noatenc_15','noatenc_16',
'usotiempo1','usotiempo2','usotiempo3','usotiempo4','usotiempo5',
'usotiempo7','usotiempo8','hor_1','hor_2','hor_3','hor_4','hor_5','hor_6',
'hor_7','hor_8','min_1','min_2','min_3','min_4','min_5','min_6','min_7',
'min_8','inst_1','inst_2','inst_3','inst_4','inst_5','inst_6','inscr_1',
'inscr_2','inscr_3', 'inscr_4','inscr_5','inscr_6','inscr_7','inscr_8',
'prob_anio','prob_mes','prob_sal', 'pareja_hog', 'conyuge_id',
'redsoc_1','redsoc_2','redsoc_3','redsoc_4','redsoc_5','redsoc_6',
'servmed_1','servmed_2','servmed_3','servmed_4','servmed_5','servmed_6',
'servmed_7','servmed_8','servmed_9','servmed_10','servmed_11',
'etnia','alfabetism', 'asis_esc', 'nivel', 'grado','segpop', 'atemed',
'aten_sal','pagoaten_1','pagoaten_2','pagoaten_3','pagoaten_4',
'pagoaten_5','pagoaten_6','pagoaten_7','hh_lug', 'mm_lug', 'hh_esp', 
'hablaind', 'lenguaind', 'hablaesp', 'comprenind', 'madre_id','padre_id', 
'mm_esp', 'usotiempo6','diabetes','pres_alta', 'peso'
]
)


# --- Viviendas ---
# we drop unused columns
# viviendas_df.columns
viviendas_df = viviendas_df.drop(columns=
[
       'mat_pared', 'mat_techos', 'mat_pisos',
       'antiguedad', 'antigua_ne', 'cocina', 'cocina_dor', 'cuart_dorm',
       'num_cuarto', 'disp_agua', 'dotac_agua', 'excusado', 'uso_compar',
       'sanit_agua', 'biodigest', 'bano_comp', 'bano_excus', 'bano_regad',
       'drenaje', 'disp_elect', 'focos_inca', 'focos_ahor', 'combustible',
       'estufa_chi', 'eli_basura', 'tenencia', 'renta', 'estim_pago',
       'pago_viv', 'pago_mesp', 'tipo_adqui', 'viv_usada', 'tipo_finan',
       'num_dueno1', 'hog_dueno1', 'num_dueno2', 'hog_dueno2', 'escrituras',
       'lavadero', 'fregadero', 'regadera', 'tinaco_azo', 'cisterna', 'pileta',
       'calent_sol', 'calent_gas', 'medidor_luz', 'bomba_agua', 'tanque_gas',
       'aire_acond', 'calefacc', 'tot_resid', 'tot_hom', 'tot_muj', 'tot_hog',
       'ubica_geo', 'ageb',  'est_socio', 'est_dis', 'upm'
]
)
# we create unique id
#viviendas_df["id_persona"] = poblacion_df.folioviv.map(str) + poblacion_df.foliohog.map(str) + poblacion_df.numren.map(str)

# --- Gastos Persona  ---
# we drop unused columns
# gastospersona_df.columns
gastospersona_df = gastospersona_df.drop(columns=
[
'tipo_gasto', 'mes_dia', 'frec_rem', 'inst', 'forma_pag1',
'forma_pag2', 'forma_pag3', 'inscrip', 'colegia',
'material', 'cantidad', 'gasto', 'costo','gasto_nm','gas_nm_tri'
]
)
# we create unique id
gastospersona_df["id_persona"] = gastospersona_df.folioviv.map(str) + gastospersona_df.foliohog.map(str) + gastospersona_df.numren.map(str)

# eliminamos columnas sobrantes
gastospersona_df = gastospersona_df.drop(columns=['folioviv', 'foliohog', 'numren','clave'])
# gastospersona_df[gastospersona_df.id_persona == '10006800415']

# We change gasto_tri to numeric
gastospersona_df['gasto_tri'] = pd.to_numeric(gastospersona_df['gasto_tri'], errors='coerce')
gastospersona_df.dtypes

# we groupby to have total gasto_tri
gastospersona_df1 = gastospersona_df.groupby(by=["id_persona"],dropna=True).sum()

# --- Ingresos  ---
# we drop unused columns
# ingresos_df.columns
ingresos_df = ingresos_df.drop(columns=
['clave', 'mes_1', 'mes_2', 'mes_3','mes_4', 'mes_5', 'mes_6', 'ing_1',
 'ing_2', 'ing_3', 'ing_4', 'ing_5','ing_6'
]
)
# we create unique id
ingresos_df["id_persona"] = ingresos_df.folioviv.map(str) + ingresos_df.foliohog.map(str) + ingresos_df.numren.map(str)

# we eliminate unused columns
ingresos_df = ingresos_df.drop(columns=['folioviv', 'foliohog', 'numren'])
# gastospersona_df[gastospersona_df.id_persona == '10006800415']

# We change ingreso_tri to numeric
ingresos_df['ing_tri'] = pd.to_numeric(ingresos_df['ing_tri'], errors='coerce')
ingresos_df.dtypes

# we groupby to have total gasto_tri (quarterly expenditure)
ingresos_df1 = ingresos_df.groupby(by=["id_persona"],dropna=True).sum()
# len(ingresos_df)
# len(ingresos_df1)

#  --- - -DATA WRANGLING- - --

# we create main table based on joining on id_persona
# len(poblacion_df)
# len(pivot)

# gastos persona
pivot = pd.merge(poblacion_df,gastospersona_df1,on="id_persona", how="left")

# ingresos
pivot = pd.merge(pivot,ingresos_df1,on="id_persona", how="left")

# vivienda
pivot = pd.merge(pivot,viviendas_df,on="folioviv", how="left")
# 257, 805

# we eliminate null income
pivot = pivot.dropna(subset=['ing_tri'])
# 175,119

# We get rid of people under 16 years of age. 
# pivot_age = pivot[["edad","ing_tri"]]
# pivot.groupby(by=["nivelaprob","edo_conyug"]).count()
# edad =pivot_age.groupby(by=["edad"],as_index=False).agg(['sum','count','mean'])
# edad.to_csv('edad.csv')
# pivot.columns
pivot1=pivot[pivot["edad"]>=16]
# 148,659

# we generate groups of variables 

# education

# make education a number
pivot1['nivelaprob'] = pd.to_numeric(pivot1['nivelaprob'], errors='coerce')

# create a list of our conditions
conditions_ed = [
(pivot1['nivelaprob'] <  3),
(pivot1['nivelaprob'] == 3),
(pivot1['nivelaprob'] == 4),
(pivot1['nivelaprob'] >  4)
]

# create a list of the values we want to assign for each condition
values_ed = ['01_Primary_and_less', '02_Middle_School', '03_High_School', '04_Higher_Education']

# create a new column and use np.select to assign values to it using our lists as arguments
pivot1['education'] = np.select(conditions_ed, values_ed)

#pivot1.groupby(by=["education","nivelaprob"]).count()


# -----------------------------------------------------
# marital_status

# make marital status a number
pivot1['edo_conyug'] = pd.to_numeric(pivot1['edo_conyug'], errors='coerce')

# create a list of our conditions
conditions_ma = [
(pivot1['edo_conyug'] == 1),
(pivot1['edo_conyug'] == 2),
(pivot1['edo_conyug'] <  6),
(pivot1['edo_conyug'] == 6)
]

# create a list of the values we want to assign for each condition
values_ma = ['01_Common_Law_Union', '02_Married', '03_No_Longer_Married', '04_Single']

# create a new column and use np.select to assign values to it using our lists as arguments
pivot1['marital_status'] = np.select(conditions_ma, values_ma)

# pivot1.groupby(by=["marital_status","edo_conyug"]).count()

# ----------------------------------------
# city_size
# make marital status a number
pivot1['tam_loc'] = pd.to_numeric(pivot1['tam_loc'], errors='coerce')

# create a list of our conditions
conditions_cs = [
(pivot1['tam_loc'] >  2),
(pivot1['tam_loc'] <= 2)
]

# create a list of the values we want to assign for each condition
values_cs = ['01_Lower_than_15000', '02_Higher_than_15000']

# create a new column and use np.select to assign values to it using our lists as arguments
pivot1['city_size'] = np.select(conditions_cs, values_cs)

# pivot1.groupby(by=["city_size","tam_loc"]).count()

# we export data
pivot1.to_csv('inegi.csv',index=False)

