#!/bin/sh
###############################################################################
# NAME: dopdf.sh
# DESCRIPTION: Script to make the final PDF file from .po file
# NOTES: Only work for this case
###############################################################################
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Variables and Constants definitions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Lists of processed files
FILES_REQUIRED_ES="emacs-lisp-intro-es.po"
FILES_REQUIRED_EN="config-local.texi emacs-lisp-intro-es.texi"
FILES_PO2TEXI="emacs-lisp-intro.texi emacs-lisp-intro-es.po"
FILES_TEXI2PDF="emacs-lisp-intro-es.po"
FILES_TEMP="fuzzies.tmp conflicts.tmp overfull.tmp underfull.tmp indextype.tmp"
TMP_EXTENSION=".vr .tp .pg .ky .fn .toc .cp .cps .aux .tmp .tmp1 .tmp2 .tmp3 .tmp4"
# Paths
PATH_WK="."
# Counters for results
CNT_TEXT_GOOD=0
CNT_TEXT_BAD=0
CNT_PDF_GOOD=0
CNT_PDF_BAD=0
#
TRACE_ON=0
ONLY_CLEAN=0
CNST_DV=754
#
CLR_NRML="\\033[0;0m"
CLR_ROJ_H="\\033[1;31m"
CLR_VER_H="\\033[1;32m"
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Definitions additionals functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Debug
# PROPOSED: Show debbuging messages to user
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: For clean output in Trace Off
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Debug(){
if [ $TRACE_ON -eq 1 -a "X$1" != "X" ]; then
   echo "$1"
fi
}
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Usage
# PROPOSED: Show messages to help user
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: Exit with error code
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Usage(){
echo "Usage: $0 [-v|-c]"
echo "   -v: Verbose mode"
echo "   -c: Only clean temporary file"
exit 1
}
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Check_Index
# PROPOSED: 
# PARAMETERS: 
# RETURN: 
# REMARKS: 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Check_Index(){
echo "Checking the index entries ..."
CNT_CIX=0
CNT_IDX_CLN=0
CNT_IDX_ML=0
STR_INDEX='#. type: cindex'
egrep "^$STR_INDEX|^msgid \"|^msgstr \"|^\"" emacs-lisp-intro-es.po > indextype.tmp
FLG_IDX=0
FLG_MSG=0
FLG_ML=0
STR_LN_ML=""
while read LINEA; do
   #echo "Leida linea: $LINEA"
   echo $LINEA | grep "^$STR_INDEX" > /dev/null
   if [ $? -eq 0 ]; then
      if [ $FLG_IDX -eq 1 -a $FLG_MSG -eq 1 ]; then
         #echo "Salgo con: [${#TXT_IDX}] $TXT_IDX"
         if [ "X$TXT_IDX" = "X" ];then
            CNT_IDX_CLN=`echo $CNT_IDX_CLN + 1 | bc`
            #echo "Cuento vacio: ${CLR_VER_H}$CNT_IDX_CLN${CLR_NRML}"
         fi
         TXT_IDX=''
         FLG_IDX=0
         FLG_MSG=0
      fi
      CNT_CIX=`echo $CNT_CIX + 1 | bc`
      if [ $TRACE_ON -eq 1 ]; then
         printf "\r  %d ${CLR_VER_H}%d${CLR_NRML} ${CLR_ROJ_H}%d${CLR_NRML}" $CNT_CIX $CNT_IDX_CLN $CNT_IDX_ML
      fi
      FLG_IDX=1
   fi
   echo $LINEA | grep "^msgstr \"" > /dev/null
   if [ $? -eq 0 -a $FLG_IDX -eq 1 ]; then
      TXT_IDX="$TXT_IDX"`echo $LINEA | cut -d'"' -f2`
      #echo "${CLR_ROJ_H}Texto${CLR_NRML}:$TXT_IDX"
      FLG_MSG=1
      FLG_ML=0
   fi
   echo $LINEA | grep "^\"" > /dev/null
   if [ $? -eq 0 -a $FLG_IDX -eq 1 -a $FLG_MSG -eq 1 ]; then
      TXT_IDX="$TXT_IDX"`echo $LINEA | cut -d'"' -f2`
      #echo "${CLR_VER_H}Texto${CLR_NRML}:$TXT_IDX"
      if [ $FLG_ML -eq 0 ]; then
         CNT_IDX_ML=`echo $CNT_IDX_ML + 1 | bc`
         #echo "Cuento multilinea: ${CLR_ROJ_H}$CNT_IDX_ML${CLR_NRML}"
         FLG_ML=1
         STR_LN_ML=`echo "$STR_LN_ML $TXT_IDX"`
      fi
   fi
   echo $LINEA | grep "^msgid \"" > /dev/null
   if [ $? -eq 0 ]; then
      if [ $FLG_IDX -eq 1 -a $FLG_MSG -eq 1 ]; then
         #echo "Salgo con: [${#TXT_IDX}] $TXT_IDX"
         if [ "X$TXT_IDX" = "X" ];then
            CNT_IDX_CLN=`echo $CNT_IDX_CLN + 1 | bc`
            #echo "Cuento vacio: ${CLR_ROJ_H}$CNT_IDX_CLN${CLR_NRML}"
         fi
         TXT_IDX=''
         FLG_IDX=0
         FLG_MSG=0
      fi
      #FLG_IDX=0
   fi
done < indextype.tmp
if [ $TRACE_ON -eq 1 ]; then
   printf "\r Examined ${CLR_ROJ_H}%d${CLR_NRML} entries from index\n" $CNT_CIX
   if [ $CNT_IDX_CLN -ne 0 ]; then
      echo "  Founded ${CLR_ROJ_H}$CNT_IDX_CLN${CLR_NRML} entries cindex empty"
   fi
   if [ $CNT_IDX_ML -ne 0 ]; then
      echo "  Founded ${CLR_ROJ_H}$CNT_IDX_ML${CLR_NRML} entries cindex multiline"
      echo -n "   Look lines: "
      for TEXTO in $STR_LN_ML; do
         printf "\n\t%s: " "$TEXTO"
      done
      printf "\n"
   fi
fi
echo "Checked the index entries."
return 0
}
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Check_Required
# PROPOSED: Check the requeriments for process
# PARAMETERS: Nothing
# RETURN: 1 if any requeriment isn't satisfied
# REMARKS: 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Check_Required(){
    if [ -f emacs-lisp-intro.texi ]; then
	echo "emacs-lisp-intro.texi found"
    else
	echo "emacs-lisp-intro.texi not found"
    fi

    if [ -f emacs-lisp-intro-es.texi ]; then
	echo "emacs-lisp-intro-es.texi found"
    else
	echo "emacs-lisp-intro-es.texi not found"
    fi

    if [ -f emacs-lisp-intro-es.po ]; then
	echo "emacs-lisp-intro-es.po found"
    else
	echo "emacs-lisp-intro-es.po not found"
    fi

    echo "Checked required files."
    return 0
}
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Clean_Temporary
# PROPOSED: Clean the all temporary files
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: Clean all except the log file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Clean_Temporary(){
echo "Cleaning temporal files ..."
for EXT in $TMP_EXTENSION; do
   for FIL in $FILES_TEXI2PDF; do
       NAME=`basename $FIL`
       rm -f ${NAME%%.po}${EXT}
       Debug " Deleted: ${NAME%%.po}${EXT}"
   done
done
for FILE in $FILES_TEMP; do
    rm -f ${FILE}
    Debug " Deleted: ${FILE}"
done
}

#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Adjust_Tables
# PROPOSED: Make som adjust to texi 
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Adjust_Tables(){
#
MULTITABLE_STR="@multitable @columnfractions"
NEW_COLFRACTION=".05 .41 .41"
#
echo " Adjustement to tables ..."
CNT_TAB=0
grep -n "$MULTITABLE_STR" emacs-lisp-intro-es.texi | while read LINEA; do
  #echo "Lei:$LINEA";
  LN=`echo "$LINEA" | cut -d: -f1`
  TC=`echo "$LINEA" | cut -d: -f2`
  echo "  In line ${CLR_VER_H}$LN${CLR_NRML} found table: ${CLR_ROJ_H}$TC${CLR_NRML}"
  cat emacs-lisp-intro-es.texi | sed -e "s/$TC/$MULTITABLE_STR $NEW_COLFRACTION/g" > emacs-lisp-intro-es.tmp
  mv emacs-lisp-intro-es.tmp emacs-lisp-intro-es.texi
  echo "   table adjusted"
done
echo " Tables adjusted."
}

#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Do_Texi_Files
# PROPOSED: Generate the texi files from the original .po files
# PARAMETERS: Nothing
# RETURN: In global variables
# REMARKS: Run po4a-translate utilitie
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Do_Texi_Files(){
    echo "Creating texi files ..."
    Debug " Command: $ po4a-translate -f texinfo -m emacs-lisp-intro.texi -p emacs-lisp-intro-es.po -l emacs-lisp-intro-es.texi -k 0.15"
    po4a-translate -f texinfo -m emacs-lisp-intro.texi -p emacs-lisp-intro-es.po -l emacs-lisp-intro-es.texi -k 0.15
    echo "Created texi files."
}
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Add_Encoding_Spanish
# PROPOSED: Insert the lines to encoding correctly for spanish language
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: Rudimentary
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Add_Encoding_Spanish(){
#
# The encoding @documentencoding "UTF-8"
# The language @documentlanguage "es"
#
echo "Creating new file emacs-lisp-intro-es.texi ..."
LIN_INI=`grep -n '@settitle' emacs-lisp-intro-es.texi | cut -d: -f1`
LIN_FIN=`grep -n '@finalout' emacs-lisp-intro-es.texi | cut -d: -f1`
LIN_TOT=`wc -l emacs-lisp-intro-es.texi | cut -d' ' -f1`
head -${LIN_INI} emacs-lisp-intro-es.texi > emacs-lisp-intro-es.tmp1
head -`echo "${LIN_FIN} - 1" | bc` emacs-lisp-intro-es.texi | tail -n`echo "${LIN_FIN} - ${LIN_INI} - 1" | bc`> emacs-lisp-intro-es.tmp2
echo "@documentencoding UTF-8" > emacs-lisp-intro-es.tmp3
echo "@documentlanguage es"  >> emacs-lisp-intro-es.tmp3
tail -n`echo "${LIN_TOT} - ${LIN_FIN} + 1" | bc` emacs-lisp-intro-es.texi > emacs-lisp-intro-es.tmp4
# Compose the final file
cat emacs-lisp-intro-es.tmp1 > emacs-lisp-intro-es.texi
cat emacs-lisp-intro-es.tmp2 >> emacs-lisp-intro-es.texi
cat emacs-lisp-intro-es.tmp3 >> emacs-lisp-intro-es.texi
cat emacs-lisp-intro-es.tmp4 >> emacs-lisp-intro-es.texi
}


#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUNCTION: Do_Adjust_to_Spanish
# PROPOSED: Make some adjusts to texi 
# PARAMETERS: Nothing
# RETURN: N/A
# REMARKS: 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Do_Adjust_to_Spanish(){
    echo "Procesing emacs-lisp-intro-es.texi ..."
# Encoding to spanish
    Add_Encoding_Spanish
#
# Adjustement table of character ASCII
    Adjust_Tables
    echo "Procesed emacs-lisp-intro-es.texi."
}


#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Main program
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
INST=`date "+%Y%m%d-%H%M%S"`
#echo "Let's go: $INST"
cd $PATH_WK
if [ $ONLY_CLEAN -eq 0 ]; then
   Check_Required
   if [ $? -ne 0 ]; then
      echo "Stoping process"
      return 1
   fi
   Do_Texi_Files
   if [ $CNT_TEXT_BAD -ne 0 ]; then
      echo "Errors ocurred when generating texi files"
      echo "Stoping process"
      return 1
   fi
   Do_Adjust_to_Spanish
fi
#

INST=`date "+%Y%m%d-%H%M%S"`
echo "Finish: $INST"
exit 0
###############################################################################
#
###############################################################################
