#!/bin/bash

# #############################################################
# v0.5
# #############################################################
# A simple benchmark script, written by
# Andreas Bauer <baueran@gmail.com>
# #############################################################
# Call with -l to avoid single ?-state-monitors to be included.
# #############################################################

# INPUT FILE
FILE=rv_properties.txt
# GNUPLOT OUTPUT FILE
GNUPLOT=gnuplot$1.dat
LTL2MON=../ltl2mon
LTL2BA=../third-party/ltl2ba
# Just an innocent counter
I=0

# REMOVE PREVIOUS GNUPLOT DATA
rm -f $GNUPLOT

# FIRST BUILD MONITORS AND COLLECT DATA
cat $FILE | grep "^\-" | sed -e 's/\-\ //g' |
while read FORM; do
   echo "-----8<---------------------------------8<-----"
   echo "Creating monitor for Phi="$FORM"."
   echo ""
   # This one only works with a specially modified LTL2BA, so ignore it!
   # PHI_SIZE=`$LTL2BA -d -f "$FORM" | grep "Size" | sed -e 's/Size:\ //g'`
   # echo "|Phi|                       :" $PHI_SIZE
   STATES_NBA=`$LTL2BA -f "$FORM" | grep "^[^ ]*:$" | wc -l`
   STATES_NNBA=`$LTL2BA -f "! ($FORM)" | grep "^[^ ]*:$" | wc -l`
   SIZE_ALPHABET=`../bin/extractalphabet "$FORM" | wc -l`
   echo "|V| from LTL2BA for Phi     :" $STATES_NBA
   let TOTAL_NBA_SIZE=$STATES_NBA*$SIZE_ALPHABET+$STATES_NBA
   echo "|V|+|E| for Phi             :" $TOTAL_NBA_SIZE
   echo ""
   echo "|V| from LTL2BA for !Phi    :" $STATES_NNBA
   let TOTAL_NNBA_SIZE=$STATES_NNBA*$SIZE_ALPHABET+$STATES_NNBA
   echo "|V+E| for !Phi              :" $TOTAL_NNBA_SIZE
   let TOTAL_SIZE_NBAS=$TOTAL_NNBA_SIZE+$TOTAL_NBA_SIZE
   echo "                            ------"
   echo "|V|+|E| for both !Phi & Phi :" $TOTAL_SIZE_NBAS
   echo "                            ======"
   MONITOR=`$LTL2MON "$FORM" --prefix .. > tmp_monitor.txt`
   TOP_STATES=`cat tmp_monitor.txt | grep "green" | wc -l`
   BOT_STATES=`cat tmp_monitor.txt | grep "red" | wc -l`
   UND_STATES=`cat tmp_monitor.txt | grep "yellow" | wc -l`
   echo ""
   echo "#\top-states from LTL2MON   :" $TOP_STATES
   echo "#\bot-states from LTL2MON   :" $BOT_STATES
   echo "#?-states from LTL2MON      :" $UND_STATES
   let MONITOR_SIZE=$TOP_STATES+$BOT_STATES+$UND_STATES
   echo "Total |V| of monitor        :" $MONITOR_SIZE
   let TOTAL_MONITOR_SIZE=$MONITOR_SIZE*$SIZE_ALPHABET+$MONITOR_SIZE
   echo "                            ------"
   echo "|V|+|E| of monitor          :" $TOTAL_MONITOR_SIZE
   echo "                            ======"
   # CHECK IF PROPERTY IS LIVENESS (activate via -l command line arg)
   if [ "$1" = "-l" ] && [ $TOP_STATES -eq 0 ] && [ $BOT_STATES -eq 0 ] 
   then
       echo "(ATTENTION: LIVENESS property will not appear in the results!)"
       continue
   fi
   # FILL LATEX DATA FILES
   echo $STATES_NBA >> states_nba.txt
   echo $TOTAL_NBA_SIZE >> total_nba_size.txt
   echo $STATES_NNBA >> states_nnba.txt
   echo $TOTAL_NNBA_SIZE >> total_nnba_size.txt
   echo $TOTAL_SIZE_NBAS >> total_size_nbas.txt
   echo $TOP_STATES >> top_states.txt
   echo $BOT_STATES >> bot_states.txt
   echo $UND_STATES >> und_states.txt
   echo $MONITOR_SIZE >> monitor_size.txt
   echo $TOTAL_MONITOR_SIZE >> total_monitor_size.txt
   echo $PHI_SIZE >> size_phi.txt
   let I=$I+1
   rm tmp_monitor.txt
   echo $I > i.txt
   # FILL GNUPLUT DATA FILES
   echo -ne $I >> $GNUPLOT
   echo -ne " " >> $GNUPLOT
   echo -ne $TOTAL_SIZE_NBAS >> $GNUPLOT
   echo -ne " " >> $GNUPLOT
   echo -ne $TOTAL_MONITOR_SIZE >> $GNUPLOT
   echo -ne " " >> $GNUPLOT
   echo $PHI_SIZE >> $GNUPLOT
done

# NOW WRITE THE LATEX OUTPUT
I=`cat i.txt`
rm i.txt
for ((i=0;i<$I;i+=1)); do 
    S=${S}"r" 
done
let I=$I-1
echo "\\documentclass{article}" > $LATEX
echo "\\begin{document}" >> $LATEX
echo "\\begin{tabular}{l|"${S}"}" >> $LATEX
echo " & \\multicolumn{"$I"}{c}{LTL specification patterns}\\\\" >> $LATEX
echo "\\hline" >> $LATEX

echo -ne "$|\\\varphi|$" >> $LATEX
cat size_phi.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo "\\hline" >> $LATEX

echo -ne "$|V|$ of $\\mathcal{A}_{\\\varphi}$" >> $LATEX
cat states_nba.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|V|+|E|$ of $\\mathcal{A}_{\\\varphi} (|\\mathcal{A}_{\\\varphi}|)$" >> $LATEX
cat total_nba_size.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|V|$ of $\\mathcal{A}_{\\\neg\\\varphi}$" >> $LATEX
cat states_nnba.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|V|+|E|$ of $\\mathcal{A}_{\\\neg\\\varphi} (|\\mathcal{A}_{\\\neg\\\varphi}|)$" >> $LATEX
cat total_nnba_size.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|\\mathcal{A}_{\\\neg\\\varphi}| + |\\mathcal{A}_{\\\varphi}|$" >> $LATEX
cat total_size_nbas.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne "\\\textbf{$CUR}" >> $LATEX
done
echo "\\\\" >> $LATEX

echo "\\hline" >> $LATEX

echo -ne "\\#$\\\top\$-states" >> $LATEX
cat top_states.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "\\#$\\\bot\$-states" >> $LATEX
cat bot_states.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "\\#?-states" >> $LATEX
cat und_states.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|V|$ of monitor" >> $LATEX
cat monitor_size.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo -ne "$|V|+|E|$ of monitor" >> $LATEX
cat total_monitor_size.txt |
while read CUR; do
    echo -ne " & " >> $LATEX
    echo -ne "\\\textbf{$CUR}" >> $LATEX
#    echo -ne $CUR >> $LATEX
done
echo "\\\\" >> $LATEX

echo "\\hline" >> $LATEX
echo "\\end{tabular}" >> $LATEX
echo "\\end{document}" >> $LATEX

# CLEAN UP TEMPORARY FILES
rm -f monitor_size.txt und_states.txt bot_states.txt 
rm -f top_states.txt states_nba.txt states_nnba.txt size_phi.txt
rm -f total_*.txt
