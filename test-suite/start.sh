#!/bin/bash

# #############################################################
# A simple benchmark and test script for the LTL3 tools
# (c) 2008 - 2009 Andreas Bauer <baueran@gmail.com>
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
   STATES_NBA=`$LTL2BA -f "$FORM" | grep "^[^ ]*:$" | wc -l`
   STATES_NNBA=`$LTL2BA -f "! ($FORM)" | grep "^[^ ]*:$" | wc -l`
   SIZE_ALPHABET=`../bin/extractalphabet "$FORM" | grep -o "(" | wc -l | sed 's/\ //g'`
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
   let I=$I+1
   rm tmp_monitor.txt
   # FILL GNUPLUT DATA FILES
   echo -ne $I >> $GNUPLOT
   echo -ne " " >> $GNUPLOT
   echo -ne $TOTAL_SIZE_NBAS >> $GNUPLOT
   echo -ne " " >> $GNUPLOT
   echo -ne $TOTAL_MONITOR_SIZE >> $GNUPLOT
   echo " " >> $GNUPLOT
done

# CLEAN UP TEMPORARY FILES
rm -f monitor_size.txt und_states.txt bot_states.txt 
rm -f top_states.txt states_nba.txt states_nnba.txt size_phi.txt
rm -f total_*.txt tmp_monitor.txt
