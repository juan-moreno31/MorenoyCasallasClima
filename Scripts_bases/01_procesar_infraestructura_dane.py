"""
==============================================================================
01_procesar_infraestructura_dane.py
==============================================================================
Procesa los módulos de la Encuesta de Educación Formal (EDUC - Formulario C600)
del DANE (Módulo VIII: TIC/Energía y Módulo I: Carátula Única de la Sede).
Genera indicadores binarios a nivel de Sede Educativa y construye la base
imputada de infraestructura para el periodo 2014-2019.

Salida: Bases_Procesadas/Infraestructura_Colegios_Imputada.csv
==============================================================================
"""

import os
import re
import subprocess
import pandas as pd
import numpy as np

BASE_DIR = r"C:\Users\diego\Documents\CongresoTunja"
DOWNLOADS_DIR = r"C:\Users\diego\Downloads"
OUTPUT_DIR = os.path.join(BASE_DIR, "Bases_Procesadas")
TEMP_EXTRACT_DIR = os.path.join(DOWNLOADS_DIR, "educ_extracted_temp")
SEVEN_ZIP = r"C:\Program Files\7-Zip\7z.exe"

os.makedirs(OUTPUT_DIR, exist_ok=True)
os.makedirs(TEMP_EXTRACT_DIR, exist_ok=True)

def extraer_con_7z(zip_path, target_dir):
    if not os.path.exists(zip_path):
        return False
    print(f"Extrayendo {os.path.basename(zip_path)} con 7-Zip...")
    cmd = [SEVEN_ZIP, "x", zip_path, f"-o{target_dir}", "-y"]
    res = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    return res.returncode == 0

def procesar_modulo_tic(filepath):
    print(f"  Procesando Módulo TIC/Infraestructura: {os.path.basename(filepath)}")
    try:
        df = pd.read_csv(filepath, encoding='latin1', low_memory=False, on_bad_lines='skip')
    except:
        df = pd.read_csv(filepath, encoding='utf-8', low_memory=False, on_bad_lines='skip')
    
    col_dane = None
    for c in df.columns:
        if c.upper() in ['SEDE_CODIGO', 'COD_ESTABLECIMIENTO', 'CODIGO_ESTABLECIMIENTO', 'COD_SEDE']:
            col_dane = c
            break
            
    if not col_dane:
        raise ValueError("No se encontró la columna del código DANE de la sede.")
        
    df['cole_cod_dane_establecimiento'] = pd.to_numeric(df[col_dane], errors='coerce')
    df = df.dropna(subset=['cole_cod_dane_establecimiento'])
    df['cole_cod_dane_establecimiento'] = df['cole_cod_dane_establecimiento'].astype('int64')

    # Búsqueda dinámica de columnas de energía y TIC
    col_e2 = next((c for c in df.columns if 'ENERGIA' in c.upper() or 'ENERGÍA' in c.upper()), None)
    col_e1 = next((c for c in df.columns if 'SIN' in c.upper() or 'E1' in c.upper()), None)
    col_tic = next((c for c in df.columns if 'TIC' in c.upper() or 'PLAN' in c.upper() or 'INTERNET' in c.upper()), None)

    df['infra_energia_red'] = df[col_e2].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_e2 else 1
    df['infra_energia_sin'] = df[col_e1].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_e1 else 0
    df['infra_plan_tic'] = df[col_tic].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_tic else 1
    
    df_sub = df[['cole_cod_dane_establecimiento', 'infra_energia_red', 'infra_energia_sin', 'infra_plan_tic']].copy()
    df_agg = df_sub.groupby('cole_cod_dane_establecimiento').max().reset_index()
    return df_agg

def procesar_modulo_caratula(filepath):
    print(f"  Procesando Módulo Carátula Única: {os.path.basename(filepath)}")
    try:
        df = pd.read_csv(filepath, encoding='latin1', low_memory=False, on_bad_lines='skip')
    except:
        df = pd.read_csv(filepath, encoding='utf-8', low_memory=False, on_bad_lines='skip')
        
    col_dane = None
    for c in df.columns:
        if c.upper() in ['SEDE_CODIGO', 'COD_ESTABLECIMIENTO', 'CODIGO_ESTABLECIMIENTO', 'COD_SEDE']:
            col_dane = c
            break
            
    if not col_dane:
        raise ValueError("No se encontró la columna del código DANE en carátula.")
        
    df['cole_cod_dane_establecimiento'] = pd.to_numeric(df[col_dane], errors='coerce')
    df = df.dropna(subset=['cole_cod_dane_establecimiento'])
    df['cole_cod_dane_establecimiento'] = df['cole_cod_dane_establecimiento'].astype('int64')

    col_pae = next((c for c in df.columns if 'PAE' in c.upper()), None)
    col_bilingue = next((c for c in df.columns if 'BILING' in c.upper()), None)
    col_etno = next((c for c in df.columns if 'ETNO' in c.upper()), None)
    col_especial = next((c for c in df.columns if 'DISCAP' in c.upper() or 'ESPECIAL' in c.upper()), None)

    df['infra_pae'] = df[col_pae].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_pae else 0
    df['infra_bilingue'] = df[col_bilingue].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_bilingue else 0
    df['infra_etnoeducativa'] = df[col_etno].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_etno else 0
    df['infra_especial'] = df[col_especial].apply(lambda x: 1 if str(x).strip() in ['1', 'Si', 'SÍ', 'SI', 'True'] else 0) if col_especial else 0
    
    cols = ['cole_cod_dane_establecimiento', 'infra_pae', 'infra_bilingue', 'infra_etnoeducativa', 'infra_especial']
    df_agg = df[cols].groupby('cole_cod_dane_establecimiento').max().reset_index()
    return df_agg

def main():
    print("=== SCRIPT 01: PROCESAMIENTO DE INFRAESTRUCTURA DANE ===")
    
    zip_tic = os.path.join(DOWNLOADS_DIR, "Opciones de respuesta de infraestructura y conectividad.zip")
    zip_caratula = os.path.join(DOWNLOADS_DIR, "Caratula unica de la sede educativa.zip")
    
    extraer_con_7z(zip_tic, TEMP_EXTRACT_DIR)
    extraer_con_7z(zip_caratula, TEMP_EXTRACT_DIR)
    
    csv_tic = os.path.join(TEMP_EXTRACT_DIR, "Opciones de respuesta de infraestructura y conectividad.CSV")
    csv_caratula = os.path.join(TEMP_EXTRACT_DIR, "Carátula única de la sede educativa.CSV")
    
    df_tic = procesar_modulo_tic(csv_tic)
    df_caratula = procesar_modulo_caratula(csv_caratula)
    
    df_merged = pd.merge(df_tic, df_caratula, on='cole_cod_dane_establecimiento', how='outer').fillna(0)
    
    # Expandir panel para 2014-2019
    anios = [2014, 2015, 2016, 2017, 2018, 2019]
    list_dfs = []
    for yr in anios:
        df_yr = df_merged.copy()
        df_yr['anio'] = yr
        list_dfs.append(df_yr)
        
    df_panel = pd.concat(list_dfs, ignore_index=True)
    
    out_file = os.path.join(OUTPUT_DIR, "Infraestructura_Colegios_Imputada.csv")
    df_panel.to_csv(out_file, index=False, encoding='utf-8')
    print(f"--> Base de infraestructura imputada guardada exitosamente en: {out_file}")

if __name__ == "__main__":
    main()
