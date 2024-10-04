df = read.csv('/Users/harith/Dropbox/ara_theta/rats_clean.csv')


### table of site richness

df_site = data.frame()
list_sites = sort(unique(df$Site))
for (i in seq_along(list_sites)){
  site = list_sites[i]
  richness_site = length(unique(df[df$Site == site,]$Gen_sp))
  temp_df = cbind.data.frame(site,richness_site)
  df_site = rbind(df_site,temp_df)
}


#### sps and sites
df_sp_sites = data.frame()

list_sps = sort(unique(df$Gen_sp))
for (i in seq_along(list_sps)){
  sp = list_sps[i]
  df_temp = df[df$Gen_sp == sp,]
  sites_n = length(df_site[df_site$site %in% df_temp$Site,]$richness_site)
  df_temp2 = cbind.data.frame(sp,sites_n)
  df_sp_sites = rbind(df_sp_sites,df_temp2)
}


### df species and sites
m=1;i=1
df_final = data.frame()
df_fina2 = data.frame()
for (n in 1:1000){
  print(n)
  list_sps = sort(unique(df$Gen_sp))
  number_random = 3:10
  for (i in seq_along(list_sps)){
    sp = list_sps[i]
    df_temp = df[df$Gen_sp == sp,]
    
    for (m in seq_along(number_random)){
      
      sites_n = length(df_site[df_site$site %in% df_temp$Site,]$richness_site)
      if(sites_n < number_random[m]){
        next
      }else{
        random_samples=sample(1:sites_n,size = number_random[m],replace = F)
        df_temp2= df_temp[random_samples,]  
        gamma = sum(df_site[df_site$site %in% df_temp2$Site,]$richness_site)
        alpha = mean(df_site[df_site$site %in% df_temp2$Site,]$richness_site)
        theta = gamma - alpha
        df_temp3 = cbind.data.frame(sp,gamma,alpha,theta,randomization = number_random[m], sites_n)
        df_final = rbind(df_final,df_temp3)
      }
    }
  }
  df_fina2_temp = cbind.data.frame(df_final,n)
  df_fina2 = rbind(df_fina2,df_fina2_temp)
}



df_final3 = df_fina2 %>% group_by(sp,randomization) %>% summarise(mean = mean(theta))




