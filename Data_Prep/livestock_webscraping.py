# In this script, I develop a web scraping algorithm to extract livestock data (in the form of tables) for all
# states (and districts) from https://epashuhaat.gov.in/?module=live_dashboard&action=surveydashboard
# to determine stocking density --> grazing pressure from domestic livestock
#Trisha Gopalakrishna

#Started on- 28/01/2021
#Last edit made on-
#Last edit made-
###############################################################################################################
#Tutorial from https://realpython.com/python-web-scraping-practical-introduction/

import requests
import pandas as pd
import os

#urban_url_list = ['https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Andhra%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Arunachal%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Assam&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Bihar&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Chandigarh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Chhattisgarh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Dadra%20&%20Nagar%20Haveli&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Delhi&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Goa&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Gujarat&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Haryana&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Himachal%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Jammu%20and%20Kashmir&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Karnataka&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Kerala&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Lakshadweep&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Madhya%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Manipur&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Nagaland&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Odisha&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Orissa&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Puducherry&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Punjab&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Rajasthan&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Sikkim&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Tamil%20Nadu&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Telangana&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Tripura&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Uttar%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_urban&state=Uttarakhand&area=urban']

#URL list differs for camels, horses & ponies and mithun & yak

#urban_url_list = ['https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Andhra%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Arunachal%20Pradesh&area=urban',
#                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Assam&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Bihar&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Chandigarh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Chhattisgarh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Dadra%20&%20Nagar%20Haveli&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Daman%20and%20Diu&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Delhi&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Goa&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Gujarat&area=urban',
#                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Haryana&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Himachal%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Jammu%20and%20Kashmir&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Jharkhand&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Karnataka&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Kerala&area=urban',
#                 #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Madhya%20Pradesh&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Manipur&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Meghalaya&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Mizoram&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Nagaland&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Odisha&area=urban',
#                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Orissa&area=urban',
#                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Puducherry&area=urban',
#                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Punjab&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Rajasthan&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Sikkim&area=urban',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Tamil%20Nadu&area=urban',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Telangana&area=urban',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Tripura&area=urban',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Uttar%20Pradesh&area=urban']
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Uttarakhand&area=urban']

#horses_ponies_urban_list=['https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Andhra%20Pradesh&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Arunachal%20Pradesh&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Assam&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Bihar&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Chandigarh&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Chhattisgarh&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Karnataka&area=urban',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Tamil%20Nadu&area=urban']

#urban_output_path= "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//Urban"

#for url in horses_ponies_urban_list:
#for url in urban_url_list:
#    html = requests.get(url).content
#    df_list = pd.read_html(html)
    #df_exotic = pd.DataFrame(df_list[0]) ##########################for cattle
    #print(df_exotic)
    #df_indigenous= pd.DataFrame(df_list[1])
    #print (df_indigenous)
    #output_path_final_exotic = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_cattle_exotic'+'.csv')
    #print(output_path_final_exotic)
    #df_exotic.to_csv(output_path_final_exotic, sep=',', na_rep='empty', index_label=False, index=False)
    #output_path_final_indigenous = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_cattle_indigenous'+'.csv')
    #print(output_path_final_indigenous)
    #df_indigenous.to_csv(output_path_final_indigenous, sep=',', na_rep='empty', index_label=False, index=False)
    #df = pd.DataFrame(df_list[0])###################################for buffalo
    #print(df)
    #output_path_final = os.path.join(urban_output_path,os.path.basename(url).split("=")[3]+'_buffalo'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',',na_rep='empty', index_label=False, index=False)
    #df_exotic = pd.DataFrame(df_list[0]) ##########################for sheep
    #print(df_exotic)
    #df_indigenous= pd.DataFrame(df_list[1])
    #print (df_indigenous)
    #output_path_final_exotic = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_sheep_exotic'+'.csv')
    #print(output_path_final_exotic)
    #df_exotic.to_csv(output_path_final_exotic, sep=',', na_rep='empty', index_label=False, index=False)
    #output_path_final_indigenous = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_sheep_indigenous'+'.csv')
    #print(output_path_final_indigenous)
    #df_indigenous.to_csv(output_path_final_indigenous, sep=',', na_rep='empty', index_label=False, index=False)
    #df = pd.DataFrame(df_list[0])###################################for goat
    #print(df)
    #output_path_final = os.path.join(urban_output_path,os.path.basename(url).split("=")[3]+'_goat'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',',na_rep='empty', index_label=False, index=False)
    #df = pd.DataFrame(df_list[0])###################################for camel
    #print(df)
    #output_path_final = os.path.join(urban_output_path,os.path.basename(url).split("=")[3]+'_camel'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',',na_rep='empty', index_label=False, index=False)
    #df = pd.DataFrame(df_list[0])###################################for horsies
    #print(df)
    #output_path_final = os.path.join(urban_output_path,os.path.basename(url).split("=")[3]+'_horses_and_ponies'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',',na_rep='empty', index_label=False, index=False)
#    df_mithun = pd.DataFrame(df_list[0]) ##########################for mithun and yak
#    print(df_mithun)
#    df_yak= pd.DataFrame(df_list[1])
#    print (df_yak)
#    output_path_final_mithun = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_mithun'+'.csv')
#    print(output_path_final_mithun)
#    df_mithun.to_csv(output_path_final_mithun, sep=',', na_rep='empty', index_label=False, index=False)
#    output_path_final_yak = os.path.join(urban_output_path, os.path.basename(url).split("=")[3]+'_yak'+'.csv')
#    print(output_path_final_yak)
#    df_yak.to_csv(output_path_final_yak, sep=',', na_rep='empty', index_label=False, index=False)

#rural_url_list = ['https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Andhra%20Pradesh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Arunachal%20Pradesh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Assam&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Bihar&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Chandigarh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Chhattisgarh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Dadra%20&%20Nagar%20Haveli&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Delhi&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Goa&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Gujarat&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Haryana&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Himachal%20Pradesh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Jammu%20and%20Kashmir&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Karnataka&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Kerala&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Lakshadweep&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Madhya%20Pradesh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Manipur&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Nagaland&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Odisha&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Orissa&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Puducherry&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Punjab&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Rajasthan&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Sikkim&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Tamil%20Nadu&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Telangana&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Tripura&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Uttar%20Pradesh&area=rural',
#                  'https://epashuhaat.gov.in/?module=live_dashboard&action=goat_rural&state=Uttarakhand&area=rural']

#URL list differs for camels, horses & ponies and mithun & yak

rural_url_list = ['https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Andhra%20Pradesh&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Arunachal%20Pradesh&area=rural'
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Assam&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Bihar&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Chandigarh&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Chhattisgarh&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Dadra%20&%20Nagar%20Haveli&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Daman%20and%20Diu&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Delhi&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Goa&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Gujarat&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Haryana&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Himachal%20Pradesh&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Jammu%20and%20Kashmir&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Jharkhand&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Karnataka&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Kerala&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Madhya%20Pradesh&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Maharashtra&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Manipur&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Meghalaya&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Mizoram&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Nagaland&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Odisha&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Orissa&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Puducherry&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Punjab&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Rajasthan&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Sikkim&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Tamil%20Nadu&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Telangana&area=rural',
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Tripura&area=rural',
                  'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Uttar%20Pradesh&area=rural']
                  #'https://epashuhaat.gov.in/?module=live_dashboard&action=mithun_rural&state=Uttarakhand&area=rural']

#horses_ponies_rural_list=['https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Andhra%20Pradesh&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Arunachal%20Pradesh&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Assam&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Bihar&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Chandigarh&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Chhattisgarh&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Karnataka&area=rural',
#                    'https://epashuhaat.gov.in/?module=live_dashboard&action=horse_rural&state=Tamil%20Nadu&area=rural']

rural_output_path= "C://Users//Trisha_Gopalakrishna//OneDrive - Nexus365//Paper2//Data//Domestic_Livestock//Rural"

#for url in horses_ponies_rural_list:
for url in rural_url_list:
    html = requests.get(url).content
    df_list = pd.read_html(html)
    #df_exotic = pd.DataFrame(df_list[0])##################for cattle
    #print(df_exotic)
    #df_indigenous= pd.DataFrame(df_list[1])
    #print (df_indigenous)
    #output_path_final_exotic = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_cattle_exotic'+'.csv')
    #print(output_path_final_exotic)
    #df_exotic.to_csv(output_path_final_exotic, sep=',', na_rep='empty', index_label=False, index=False)
    #output_path_final_indigenous = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_cattle_indigenous'+'.csv')
    #print(output_path_final_indigenous)
    #df_indigenous.to_csv(output_path_final_indigenous, sep=',', na_rep='empty', index_label=False, index=False)
    #df=pd.DataFrame(df_list[0]) #########################for buffalo
    #print (df)
    #output_path_final = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_buffalo'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',', na_rep='empty', index_label=False, index=False)
    #df_exotic = pd.DataFrame(df_list[0])##################for sheep
    #print(df_exotic)
    #df_indigenous= pd.DataFrame(df_list[1])
    #print (df_indigenous)
    #output_path_final_exotic = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_sheep_exotic'+'.csv')
    #print(output_path_final_exotic)
    #df_exotic.to_csv(output_path_final_exotic, sep=',', na_rep='empty', index_label=False, index=False)
    #output_path_final_indigenous = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_sheep_indigenous'+'.csv')
    #print(output_path_final_indigenous)
    #df_indigenous.to_csv(output_path_final_indigenous, sep=',', na_rep='empty', index_label=False, index=False)
    #df=pd.DataFrame(df_list[0]) #########################for goat
    #print (df)
    #output_path_final = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_goat'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',', na_rep='empty', index_label=False, index=False)
    #df=pd.DataFrame(df_list[0]) #########################for camel
    #print (df)
    #output_path_final = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_camel'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',', na_rep='empty', index_label=False, index=False)
    #df=pd.DataFrame(df_list[0]) #########################for horses and ponies
    #print (df)
    #output_path_final = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_horses_and_ponies'+'.csv')
    #print (output_path_final)
    #df.to_csv(output_path_final, sep=',', na_rep='empty', index_label=False, index=False)
    df_mithun = pd.DataFrame(df_list[0])##################for mithun and yak
    print(df_mithun)
    df_yak= pd.DataFrame(df_list[1])
    print (df_yak)
    output_path_final_mithun = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_mithun'+'.csv')
    print(output_path_final_mithun)
    df_mithun.to_csv(output_path_final_mithun, sep=',', na_rep='empty', index_label=False, index=False)
    output_path_final_yak = os.path.join(rural_output_path, os.path.basename(url).split("=")[3]+'_yak'+'.csv')
    print(output_path_final_yak)
    df_yak.to_csv(output_path_final_yak, sep=',', na_rep='empty', index_label=False, index=False)

