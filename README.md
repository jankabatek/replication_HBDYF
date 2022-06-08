# Replication code for [Kabátek (2021), ILR](https://journals.sagepub.com/doi/full/10.1177/0019793919897914)
       
<img src="https://www.jankabatek.com/img/HBDYF.jpg" alt="drawing" width="600"/>         
    
**Package details**
    
This replication package contains one Stata file that constructs the principal analytiucal dataset, estimates the full set of job separation models, and produces select desecriptive statistics presented in Kabátek (2021). The code for the remainder of descriptive statistics and regression models is available on request, feel free to reach out! 

The code was written and executed in STATA 15.0, OS Windows 8.            
The code uses an (optional) PLOTTABS package that can be downloaded from [here](https://github.com/jankabatek/statapack). 

To operationalize the code, please change the global MAIN_FOL macro to your preferred project directory.                                              
                                                                              
The code is commented and it contains additional information that should facilitate the replication efforts.  

---

The analysis draws on proprietary data, which means that **the datasets are not supplied in the replication package**.                                 
 
To execute the code with proprietary STATNL data, make sure that you have access to the following datasets: 
                            
      BAANSOMMENTAB (x)
      BAANKENMERKENTAB (x)                       
      GBAPERSOONTAB (x)                         
      SPOLISBUS                            

The datasets marked with (x) are essential. The other datasets add valuable information and can be used to derive additional descriptive statistics, however the code runs without them. Inquiries regarding the STATNL data access should be addressed to [microdata@cbs.nl](mailto:microdata@cbs.nl)

The regression model results are stored in the log file stored in folder MAIN_FOL/LOG/log_VERSION  
The runtime of the analysis is several hours.                                                                       
