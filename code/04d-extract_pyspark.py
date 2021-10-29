from pyspark.sql import SparkSession
spark = SparkSession.builder.master("local[4]") \
                    .appName('sparklyr') \
                    .getOrCreate()

print(spark.catalog.listTables())

# Cargar datos
flights = spark.read.csv('../data/flights/flights.csv', header=True)
flights.createOrReplaceTempView("flights")

airports = spark.read.csv('../data/flights/airports.csv', header=True)
airports.createOrReplaceTempView("airports")

print(spark.catalog.listTables())

flights2 = spark.table('flights')

# show()

########################################
# Filtros ------------------------------
# - Python-like -  `filter()`
# - SQL syntax -  `WHERE`

flights.filter(flights.air_time > 120).show(6)

flights.filter("air_time > 120").show(6)

# all the flights that flew over 1000  miles two ways.

flights.filter("distance > 1000").show(6)

######################################################
# Selección de columnas ------------------------------

flights.select("tailnum", "origin", "dest").show(6)

small_selection = flights.select(flights.origin, 
                                 flights.dest, 
                                 flights.carrier)

selection_filtered = small_selection.filter(flights.dest == "PDX")

selection_filtered.show(6)

# Operaciones por columnas
flights.select((flights.air_time/60).alias('duration_hrs')).show(10)
flights.selectExpr('air_time/60 as duration_hrs').show(10)

avg_speed = (flights.distance/(flights.air_time/60)).alias("avg_speed")
flights.select("origin", "dest", "tailnum", avg_speed).show(6)

# With SQL expression
flights.selectExpr("origin", "dest", "tailnum", 
                   "distance/(air_time/60) as avg_speed").show(6)

######################################################
# Agregación -----------------------------------------

from pyspark.sql.types import DoubleType
flights = flights.withColumn('distance', flights['distance'].cast(DoubleType()))
flights = flights.withColumn('air_time', flights['air_time'].cast(DoubleType()))

print('Shortest flight from JFK in terms of distance')
flights.filter(flights.origin == 'JFK').groupBy().min('distance').show()

print('Longest flight from JFK in terms of air time')
flights.filter(flights.origin == 'JFK').groupBy().max('air_time').show()


# Average duration of Delta flights
flights.filter(
    flights.carrier == "DL").filter(
    flights.origin == "LGA").groupBy(
    ).avg("air_time").show(6)

# Total hours in the air
flights.withColumn("duration_hrs", 
                   flights.air_time/60).groupBy(
).sum("duration_hrs").show()


# Group by tailnum
by_plane = flights.groupBy("tailnum")
by_plane.count().show(6)


# Group by origin
by_origin = flights.groupBy("origin")
by_origin.avg("air_time").show()

# The `.agg()` method
import pyspark.sql.functions as F

# Group by month and dest
by_month_dest = flights.groupBy("month", "dest")

# Standard deviation of departure delay
by_month_dest.agg(F.stddev('dep_delay')).show(6)

#################################################
# Joining --------------------------------------

airports = airports.withColumnRenamed('faa', 'dest')
flights_with_airports = flights.join(airports, on='dest', how='leftouter')

print(flights_with_airports.show(6))
