Overview
--------

RDataXMan (**R** **D**ata e**X**traction **M**anagement) is an Open Source tool built using the R language, with the capability to assist users perform reproducible extractions of datasets using a simple to use template approach. The R package is a used in conjunction with a user-friendly graphical user interface (GUI) based on the R Commander framework that assists the user from the identification of data or columns, to the full extraction of research data. Our aim in the development of this tool was to lower the barrier of entry and speed up efforts to access a variety of data sources for research, while promoting reproducibility and minimizing the risk of data extraction variation. 

The RDataXMan package and the [R Commander plug-in](https://github.com/nyilin/RcmdrPlugin.RDataXMan) are free under an academic non-commercial license, and operates on Windows and Mac operating systems. Installation of this application is described below, and detailed instructions on the use of RDataXMan are available in **"User Manual.pdf"**.

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
