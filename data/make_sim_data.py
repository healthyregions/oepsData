import pandas as pd
import random

TOTAL_RESPONSES = 110

## Zip Code Pools
# zips_adj -- 60621 (our loc), and adjacent places
# zips_cluster -- area we expect to see another cluster of usage in
# zips_flavor -- we grab a few choices from these zips just to add flavor.
#zips_adj = [60621, 60636, 60637, 60609, 60615, 60620, 60619]
#zips_cluster = [60628, 60643, 60633, 60655, 60827]
#zips_flavor = [60611, 60639, 60707, 60641, 60640, 60657, 60631, 60602, 60604, 60661]

zips_core = [22801, 22802] # 60%
zips_ext = [22815, 22853, 22812] # 20%
zips_extext = [22841, 24441] # 10%
zips_all = [
  26802, 26812, 26815, 26836, 22850,
  24441, 22853, 22932, 22935, 24471, 
  22940, 22973, 24486, 22801, 22802,
  22807, 22811, 22812, 22815, 22820,
  22821, 22827, 22830, 22831, 22832,
  22834, 22840, 22841, 22842, 22843,
  22844, 22846, 22849
] # 20% scatterred randomly

## Get zips
sim_zips = random.choices(zips_core, k=60) \
    + random.choices(zips_ext, k=20) \
    + random.choices(zips_extext, k=10) \
    + random.choices(zips_all, k=20)

income = random.choices([1, 0], weights=[.3, .7], k=60) \
    + random.choices([1, 0], weights = [.5, .5], k=20) \
    + random.choices([1, 0], weights= [.2, .8], k=10) \
    + random.choices([1, 0], weights=[.15, .85], k=20)
    
# has_car should be BASICALLY the same as fin_aid lol
has_car = random.choices([1, 0], weights=[.3, .7], k=60) \
    + random.choices([1, 0], weights = [.5, .5], k=20) \
    + random.choices([1, 0], weights= [.2, .8], k=10) \
    + random.choices([1, 0], weights=[.15, .85], k=20)
    
## Generate PCP
pcp = random.choices([1, 0], weights=[0.8, 0.2], k=TOTAL_RESPONSES)

specialists = []
for val in pcp:
    if val == 1:
        sp = random.choices([1, 0], weights=[.6, .4])[0]
        specialists.append(sp)
    else:
        # all data must have a 1 on either SP or PCP
        specialists.append(1)

data = pd.DataFrame({
    "ZCTA": sim_zips,
    "Fin_Aid": income,
    "Has_Pcp": pcp,
    "Has_Sp" : specialists,
    "Has_Car": has_car
})

data = data.sample(frac=1, replace=False)
data['RID'] = [f"R{str(i).rjust(4, '0')}" for i in range(TOTAL_RESPONSES)]
data = data[['RID', 'ZCTA', 'Fin_Aid', 'Has_Pcp', 'Has_Sp', "Has_Car"]]
print(data)
data.to_csv('data/survey_data.csv', index=False)
