Overview
--------

RDataXMan (**R** **D**ata e**X**traction **M**anagement) is an Open Source tool built using the R language, with the capability to assist users perform reproducible extractions of datasets using a simple to use template approach. The R package is a used in conjunction with a user-friendly graphical user interface (GUI) based on the R Commander framework that assists the user from the identification of data or columns, to the full extraction of research data. Our aim in the development of this tool was to lower the barrier of entry and speed up efforts to access a variety of data sources for research, while promoting reproducibility and minimizing the risk of data extraction variation. 

The RDataXMan package and the [R Commander plug-in](https://github.com/nyilin/RcmdrPlugin.RDataXMan) are free under an academic non-commercial license, and operates on Windows and Mac operating systems. Installation of this application is described below, and detailed instructions on the use of RDataXMan are available in **"User Manual.pdf"**. The simulated datasets used in the illustrative example described in the user manual are available from https://github.com/nyilin/RDataXMan_example_data.

Installation
------------

Please read the user manual (**"User Manual.pdf"**) for detailed instructions.

Installation of RDataXMan requires the installation of Java JDK:

1.  Go to https://www.oracle.com/technetwork/java/javase/downloads/index.html
2.  Go to the download page for the installer of latest Java JDK by
    following the “JDK Download” link.
3.  Download the appropriate installer.
    -   Windows users should choose “Windows x64 Installer”.
    -   macOS users should choose “macOS Installer”.

After successfully installing Java JDK, macOS users need to configure Java by 
executing the following commands in the Terminal:

    sudo R CMD javareconf –n
    sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

Installation and configuration of Java is successful if users are able to install and load the rJava package, by executing the following commands in RStudio without error:

    install.packages("rJava")
    library(rJava)

After the steps above, both Windows and macOS users can use the
following code to install the RDataXMan package from GitHub:

    # Package devtools is needed to install from GitHub
    # install.packages("devtools")
    devtools::install_github("nyilin/RDataXMan")

R Script for Illustrative Example
---------------------------------

The R script for the illustrative example provided in the user manual is
provided below:

```r
# Users should modify the value of 'wkdir' below to the actual path to their working directory:
wkdir <- "C:/Users/username/Documents/RDataXMan"
setwd(wkdir)

initWkdir(wkdir)
initResearchFolder(wkdir,"QoL Study")

genInclusion(wkdir = wkdir, research.folder = "QoL Study",
             table_name = "QoL survey data.xlsx",
             key.var = "PATIENT_NRIC", key.desc = c(),
             identifier.var = c('PATIENT_NRIC'),
             count = "TRUE", data.type = "flat", overwrite = TRUE,
             username = "", password = "", database = "private")
genVariable(wkdir = wkdir, research.folder = "QoL Study",
            table_name  = "QoL survey data.xlsx",
            identifier.var = c('PATIENT_NRIC'), omit.var = c(), data.type = "flat",
            overwrite = TRUE,
            username = "", password = "", database = "private")

genInclusion(wkdir = wkdir, research.folder = "QoL Study",
             table_name = "v2m_c_movement_pc_3yr",
             key.var = "AYEAR", key.desc = c(),
             identifier.var = c('PATIENT_NRIC', 'CASE_NO'),
             count = "TRUE", data.type = "sql", overwrite = TRUE,
             # Please update the value of 'username', 'password' and 'database' accordingly:
             username = "root", password = "sqlpwd", database = "emr")
genVariable(wkdir = wkdir, research.folder = "QoL Study",
            table_name  = "v2m_c_movement_pc_3yr",
            identifier.var = c('PATIENT_NRIC','CASE_NO'), omit.var = c('ADATE','DDATE'),
            data.type = "sql", overwrite = TRUE,
            # Please update the value of 'username', 'password' and 'database' accordingly:
            username = "root", password = "sqlpwd", database = "emr")

genVariable(wkdir = wkdir, research.folder = "QoL Study",
            table_name  = "v2m_c_diagnosis_p_3yr",
            identifier.var = c('PATIENT_NRIC','CASE_NO'),
            omit.var = c('DIAGNOSIS_DATE'), data.type = "sql", overwrite = TRUE,
            # Please update the value of 'username', 'password' and 'database' accordingly:
            username = "root", password = "sqlpwd", database = "emr")
genVariable(wkdir = wkdir, research.folder = "QoL Study",
            table_name  = "v2m_c_patient_basic_3yr",
            identifier.var = c('PATIENT_NRIC'), omit.var = c('DEATH_DATE'),
            data.type = "sql", overwrite = TRUE,
            # Please update the value of 'username', 'password' and 'database' accordingly:
            username = "root", password = "sqlpwd", database = "emr")

rdataxman_result <- extract_data(
  wkdir = wkdir, research.folder = "QoL Study",
  # Please update the file names below accordingly:
  inclusion.xls.file =
    c('inclusion.QoL survey data.xlsx_PATIENT_NRIC_xlsx.xls',
      'inclusion.v2m_c_movement_pc_3yr_AYEAR_sql_root_20210119_161712.xls'),
  variable.xls.file =
    c('variable.QoL survey data.xlsx(PATIENT_NRIC)_xlsx.xls',
      'variable.v2m_c_diagnosis_p_3yr_(PATIENT_NRIC_CASE_NO)_sql_root_20210119_162009.xls',
      'variable.v2m_c_movement_pc_3yr_(PATIENT_NRIC_CASE_NO)_sql_root_20210119_161818.xls',
      'variable.v2m_c_patient_basic_3yr_(PATIENT_NRIC)_sql_root_20210119_162049.xls'),
  dataLogic = "Intersection", select.output = c('1','2','4'), overwrite = TRUE,
  # Please update the value of 'username', 'password' and 'database' accordingly:
  username = "root", password = "sqlpwd", database = "emr"
)
```
