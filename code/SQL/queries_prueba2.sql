select * from transacciones filter 

CREATE TABLE septiembre as 
SELECT * FROM transacciones 
WHERE strftime('%m', T071_DAT_OPERATION) = '09' and strftime('%Y', T071_DAT_OPERATION) = '2015' 

select count(*) from septiembre

CREATE TABLE cat_client_sept AS
SELECT HASH_MAP_CU, max(T071_DAT_OPERATION) as FECHA_PRIM_SURT from septiembre
	group by HASH_MAP_CU;
	
select * from cat_client_sept limit 100
select * from transacciones limit 100

CREATE TABLE trans_prueba_2 AS 
SELECT HASH_MAP_CTA, 
		       HASH_MAP_CU,
		       T071_CODE,
		       T071_AMOUNT,
		       T071_DAT_OPERATION,
		       T071_TIM_OPERATION FROM (
		SELECT * FROM cat_client_sept c
		LEFT JOIN transacciones t		 
		ON t.HASH_MAP_CU = c.HASH_MAP_CU);
		
CREATE TABLE trans_prueba_2 AS 
		SELECT * FROM cat_client_sept c
		LEFT OUTER JOIN transacciones t		 
		ON t.HASH_MAP_CU = c.HASH_MAP_CU;
		
create table trans_prueba_sept2 as 
select * from
(select HASH_MAP_CTA, 
		       HASH_MAP_CU,
		       T071_CODE,
		       T071_AMOUNT,
		       T071_DAT_OPERATION,
		       T071_TIM_OPERATION ,
		       FECHA_PRIM_SURT from trans_prueba_sept)
where FECHA_PRIM_SURT is not null

select count(*) from trans_prueba_sept2

create table trans_prueba_sept3 as 
select * from
(select HASH_MAP_CU,
	  T071_AMOUNT,
	  T071_DAT_OPERATION from trans_prueba_sept2)
where strftime('%Y', T071_DAT_OPERATION) = '2014'  or strftime('%Y', T071_DAT_OPERATION) = '2015' 

select count(*) from trans_prueba_sept3

--Pegamos la clave hasheada a la tabla de personas con crédito
create view claves_cred_hash as
select HASH_MAP_CU as HMCU from 
	(prim_surt_cred a left join catalogo_hash b
	on a.NPAIS = b.Pais and
	     a.NCANAL = b.Canal and
	     a.NSUCURSAL = b.Sucursal and 
	     a.NFOLIO = b.Folio)
	     
--Pegamos las claves de las personas con crédito que están en la base de personas de septiembre 
create view claves_capta_sept as 
select HASH_MAP_CU as HMCU2 from
	(cat_client_sept a left join claves_cred_hash b
	on a.HASH_MAP_CU = b.HMCU)
where HMCU is null

--Nos quedamos con las transacciones de las personas adecuadas para la prueba
create table transacciones_prueba_sept_4 as 
select HASH_MAP_CU,
	  T071_AMOUNT,
	  T071_DAT_OPERATION from 
	(transacciones_prueba_sept3 a left join claves_capta_sept b
	on a-HASH_MAP_CU = b.HMCU2)
where HMCU2 is not null

select count(*) from transacciones_prueba_sept_4
select max(T071_DAT_OPERATION) as maximo, 
	  min(T071_DAT_OPERATION) as minimo  from transacciones_prueba_sept_4
select * from transacciones_prueba_sept_4 LIMIT 10

create table trans_prueba_sept5 as
select * from transacciones_prueba_sept_4
where T071_DAT_OPERATION <= '2015-09-30'

select HASH_MAP_CU, count(*) as cuenta from trans_prueba_sept5 group by HASH_MAP_CU order by cuenta desc

select T071_CODE, count(*) as cuenta from transacciones where HASH_MAP_CU = 'Ull6lVwryKLaal8Tf1UCXw==' group by T071_CODE order by cuenta desc
select * from catalogo_hash where HASH_MAP_CU = 'Ull6lVwryKLaal8Tf1UCXw=='

CREATE TABLE atipicos_prueba_sept (
	"HASH_MAP_CU" VARCHAR(20), 
	"CUENTA" INT 
);

select count(*) from atipicos_prueba_sept