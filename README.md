## RDataXMan Software Requirements for Apple Mac Computers

For installation of the software on Apple computers running OS X please follow the steps before attempting to install RDataXMan

1. Install the latest version of Oracle Java JDK from https://www.oracle.com/technetwork/java/javase/downloads/index.html
2. Open the terminal and type the following commands:  
    a. `sudo R CMD javareconf –n`  
    b. `sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib`  
3. Install package `rJava` from CRAN
