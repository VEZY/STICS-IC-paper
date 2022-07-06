#!/bin/bash

# initial dir
INIT_DIR=`pwd`
 
# set version
VER=$1
if [ -z "$1" ];
  then
     echo "Missing first input argument for model version !"
     exit 1
fi

# set stics dir
STICS_DIR="$2"
DIR=$STICS_DIR/Debug
if [ -z "$2" ];
  then
     DIR=Debug
     STICS_DIR=".";
fi

# version file creation
DATE=`date '+%Y-%m-%d'`
TIME=`date '+%H:%M:%S'`

# getting revision date
DATEVER=$3
if [ -z "$3" ];
  then
     DATEVER=$DATE
fi

echo "Informations about Stics model build version" > $STICS_DIR/stics_version.txt
echo "Build date : $DATE ($TIME)" >> $STICS_DIR/stics_version.txt
echo "Version : $VER" >> $STICS_DIR/stics_version.txt
echo "Version date : $DATEVER" >> $STICS_DIR/stics_version.txt

# checking OS type
OSNAME="Linux"
UNAME_S=`uname -s`
if [ $UNAME_S = "Darwin" ];
	then
	OSNAME="Mac OS"
fi
echo "OS name : $OSNAME" >> $STICS_DIR/stics_version.txt


# Creating F90 file with version and date
TMP=$STICS_DIR"/src/inputs/tmp"
# if dir name needed
if ! [ -e "`pwd`/$TMP" ]
   then
	echo "Invalid directory :`pwd`/$TMP"
        echo "Set src parent directory name as script second input argument"
        exit 1
fi

# Creating F90 file with version and date
if [ -e $TMP/call_num_version.f90 ];
   then
      rm -f $TMP/call_num_version.f90
fi

cat $TMP/call_num_version_head.tmp > $TMP/call_num_version.f90
echo "        nomversion='$VER'" >> $TMP/call_num_version.f90
echo "        dateversion='$DATEVER'" >> $TMP/call_num_version.f90
cat $TMP/call_num_version_foot.tmp >> $TMP/call_num_version.f90
mv $TMP/call_num_version.f90 $STICS_DIR/src/inputs


# moving to Debug folder for compilation
cd $DIR

if [ -e Stics ];
  then
    rm Stics
fi 

# link to gfortran version, specific to PIC 147.100.65.20
which gfortran
r=$?
if [ $r=="1" ]
  then
    ln -s /usr/bin/gfortran-4.4 ./gfortran
    curr=`pwd`
    PATH=$PATH:$curr
fi


make all

if ! [ "$?" == "0" ]; then exit 1;fi

if ! [ -e Stics ]; then exit 1;fi

# back to initial dir
cd $INIT_DIR
