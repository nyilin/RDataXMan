Overview
--------

Before installing the RDataXMan package, please make sure the latest
version of Oracle Java JDK has been installed:

1.  Go to
    <a href="https://www.oracle.com/java/technologies/javase-jdk15-downloads.html" class="uri">https://www.oracle.com/java/technologies/javase-jdk15-downloads.html</a>
2.  Go to the download page for the installer of latest Java JDK by
    following the “JDK Download” link.
3.  Download the appropriate installer.
    -   Windows users should choose “Windows x64 Installer”.
    -   macOS users should choose “macOS Installer”.

After successfully installing Java JDK, macOS users need to configure Java by 
executing the following commands in the Terminal:

    sudo R CMD javareconf –n
    sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

After the steps above, both Windows and macOS users can use the
following code to first install the rJava package from CRAN and then
install the RDataXMan package from GitHub:

    install.packages("rJava")
    # Package devtools is needed to install from GitHub
    # install.packages("devtools")
    devtools::install_github("nyilin/RDataXMan")
