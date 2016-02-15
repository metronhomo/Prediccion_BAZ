# Selecciona las primeras 10 columnas y ñas guarda en un archivo
cut -f 1-10 ../../data/Bases\ originales/MUESTRA\ 1_500\ MIL/captacion\ sociodemograficos.txt > ../../output/Modelos/captacion_sociodemograficos.tsv
#cut -f 1-10 ../../data/Bases\ originales/MUESTRA\ 1_500\ MIL/captacion\ sociodemograficos_2.txt > ../../output/Modelos/captacion_sociodemograficos.tsv
echo 'cut listo'

# Hace el CREATE statement para el archivo creado (el de 10 columnas)
csvsql -t -i sqlite ../../output/Modelos/captacion_sociodemograficos.tsv > ../../code/modelos/create_statement_captacion_sociodemograficos.sql
echo 'ya hizo CREATE statement'

# Quita el header al archivo creado (el de 10 columnas)
sed -i 1d ../../output/Modelos/captacion_sociodemograficos.tsv
echo 'ya quitó header'

# Crea la tabla en la base de datos de SQLite
sqlite3 ../../data/SQL/BAZ.db < ../../code/modelos/create_statement_captacion_sociodemograficos.sql
echo 'ya créó tabla'

# Inserta los datos en la base de SQLite
sqlite3 ../../data/SQL/BAZ.db < ../../code/modelos/commands_insert_captacion_sociodemograficos.txt
echo 'ya insertó'

# Como esto se ejecutó con permisos de root, seguramente la base final no va a tener permisos. Aquí se los cambiamos:
sudo chmod 777 ../../data/SQL/BAZ.db
echo 'ya cambió permisos'

echo 'TODO LISTO!! :D'