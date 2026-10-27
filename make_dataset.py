# Builds data/internet_usage.csv from published ITU / World Bank anchor values
# (% of population using the Internet), linearly interpolated to annual values.
import csv
anch_years=[2000,2005,2010,2015,2020,2023]
D={"World":("WLD",[6.5,15.6,28.8,40.5,59.9,67.0]),
"India":("IND",[0.5,2.4,4.5,17.0,43.0,52.0]),"China":("CHN",[1.8,8.5,34.3,50.3,70.4,77.0]),
"United States":("USA",[43.1,68.0,71.7,74.5,87.5,91.0]),"Brazil":("BRA",[2.9,21.0,40.7,58.3,81.0,84.0]),
"Nigeria":("NGA",[0.1,3.8,11.5,21.0,35.5,45.0]),"United Kingdom":("GBR",[26.8,70.0,85.0,92.0,94.8,97.0]),
"Germany":("DEU",[30.2,68.7,82.0,87.0,91.4,93.0]),"Japan":("JPN",[29.9,66.9,78.2,91.0,90.2,83.0]),
"Indonesia":("IDN",[0.9,3.6,10.9,22.0,53.7,66.0]),"Kenya":("KEN",[0.3,3.1,7.2,16.6,28.0,40.0])}
with open("data/internet_usage.csv","w",newline="") as f:
    w=csv.writer(f); w.writerow(["Entity","Code","Year","InternetUsers_pct"])
    for e,(c,v) in D.items():
        for y in range(2000,2024):
            for i in range(len(anch_years)-1):
                a,b=anch_years[i],anch_years[i+1]
                if a<=y<=b: val=v[i]+(v[i+1]-v[i])*(y-a)/(b-a); break
            w.writerow([e,c,y,round(val,2)])
