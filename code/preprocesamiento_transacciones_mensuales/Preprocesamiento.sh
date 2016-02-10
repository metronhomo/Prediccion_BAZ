tail -n +2 Captacion1.txt > Captacion1_tail.txt
tail -n +2 Captacion2.txt > Captacion2_tail.txt
cat Captacion1_tail.txt Captacion2_tail.txt > Captacion.txt
awk '{print "0|"$0}' Captacion.txt > Captacion_variable_respuesta.txt 
tail -n +2 Credito.txt > Credito_tail.txt
awk '{print "1|"$0}' Credito_tail.txt > Credito_variable_respuesta.txt 
cat Credito_variable_respuesta.txt Captacion_variable_respuesta.txt > datos.txt


