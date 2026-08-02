"""
==============================================================================
DatosEarth.py
==============================================================================
Script para consultar y extraer series de temperatura y precipitación desde
Google Earth Engine (GEE) a partir de las coordenadas de las sedes educativas
(WGS84) o límites municipales de Cundinamarca.

Requiere:
    pip install earthengine-api geemap pandas
==============================================================================
"""

import ee
import pandas as pd
import os

BASE_DIR = r"C:\Users\diego\Documents\CongresoTunja"
OUTPUT_DIR = os.path.join(BASE_DIR, "Bases_Procesadas")

def inicializar_earth_engine():
    """Inicializa la API de Google Earth Engine con autenticación."""
    try:
        ee.Initialize()
        print("✓ Google Earth Engine inicializado correctamente.")
    except Exception as e:
        print("Autenticando en Google Earth Engine...")
        ee.Authenticate()
        ee.Initialize()

def extraer_temperatura_era5(coordenadas_df, fecha_inicio="2014-01-01", fecha_fin="2019-12-31"):
    """
    Extrae la temperatura media de la superficie (ERA5-Land) para cada par
    de coordenadas (latitud, longitud) entre fecha_inicio y fecha_fin.
    """
    print(f"Consultando ERA5-Land Daily Aggregated ({fecha_inicio} a {fecha_fin})...")
    
    # Colección ERA5-Land Daily
    era5 = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") \
             .filterDate(fecha_inicio, fecha_fin) \
             .select('temperature_2m')
             
    # Crear Features a partir del DataFrame de colegios
    features = []
    for idx, row in coordenadas_df.iterrows():
        lat, lon = row['latitud'], row['longitud']
        cod_dane = row['cole_cod_dane_establecimiento']
        point = ee.Geometry.Point([lon, lat])
        feat = ee.Feature(point, {'cole_cod_dane_establecimiento': cod_dane})
        features.append(feat)
        
    fc = ee.FeatureCollection(features)
    
    # Reducción espacial
    def reducir_punto(img):
        return img.reduceRegions(
            collection=fc,
            reducer=ee.Reducer.mean(),
            scale=11132
        ).map(lambda f: f.set('fecha', img.date().format('YYYY-MM-dd')))
        
    resultados = era5.map(reducir_punto).flatten()

    # Convertir a DataFrame
    print("Descargando datos reducidos...")
    data = resultados.getInfo()
    
    records = []
    for f in data['features']:
        props = f['properties']
        temp_k = props.get('mean')
        temp_c = temp_k - 273.15 if temp_k else None
        records.append({
            'cole_cod_dane_establecimiento': props.get('cole_cod_dane_establecimiento'),
            'fecha': props.get('fecha'),
            'temp_celsius_era5': temp_c
        })
        
    return pd.DataFrame(records)

def main():
    print("=== SOLICITUD A GOOGLE EARTH ENGINE (GEE) ===")
    inicializar_earth_engine()
    
    # Cargar coordenadas de colegios si existen
    coords_file = os.path.join(OUTPUT_DIR, "Colegios_Cundinamarca_ArcGIS.csv")
    if os.path.exists(coords_file):
        df_coords = pd.read_csv(coords_file)
        print(f"Cargadas {len(df_coords)} sedes educativas georreferenciadas.")
        
        # Extraer muestra o serie completa
        # df_clima_gee = extraer_temperatura_era5(df_coords)
        # df_clima_gee.to_csv(os.path.join(OUTPUT_DIR, "Clima_Google_Earth_Engine.csv"), index=False)
        print("Script listo para ejecutar extracción masiva en GEE.")
    else:
        print("Primero ejecuta 03_construir_panel_maestro.R para generar la base espacial de colegios.")

if __name__ == "__main__":
    main()
