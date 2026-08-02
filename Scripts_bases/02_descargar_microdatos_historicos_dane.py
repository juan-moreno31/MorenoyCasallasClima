"""
==============================================================================
02_descargar_microdatos_historicos_dane.py
==============================================================================
Realiza el scraping y la descarga directa de las bases microdatos de Educación
Formal (EDUC C600) para la serie histórica 2014-2019 desde el catálogo DANE ANDA
(IDs 502, 500, 503, 786, 615, 669).

Salida: Bases_Procesadas/dane_educ_historico/
==============================================================================
"""

import os
import re
import zipfile
import urllib.request
import subprocess

BASE_DIR = r"C:\Users\diego\Documents\CongresoTunja"
HISTORICO_DIR = os.path.join(BASE_DIR, "Bases_Procesadas", "dane_educ_historico")
SEVEN_ZIP = r"C:\Program Files\7-Zip\7z.exe"
os.makedirs(HISTORICO_DIR, exist_ok=True)

CATALOG_IDS = {
    2014: 502,
    2015: 500,
    2016: 503,
    2017: 786,
    2018: 615,
    2019: 669
}

HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36',
    'Referer': 'https://microdatos.dane.gov.co/index.php/catalog'
}

def obtener_enlaces_descarga(cat_id):
    url = f"https://microdatos.dane.gov.co/index.php/catalog/{cat_id}/related-materials"
    urls_descarga = []
    try:
        req = urllib.request.Request(url, headers=HEADERS)
        html = urllib.request.urlopen(req).read().decode('utf-8', errors='ignore')
        matches = re.findall(r'href=["\'](https?://microdatos\.dane\.gov\.co/index\.php/catalog/\d+/download/\d+)["\']', html)
        urls_descarga = list(set(matches))
    except Exception as e:
        print(f"Error extrayendo enlaces para catálogo {cat_id}: {e}")
    return urls_descarga

def descargar_archivo(url, dest_path):
    try:
        req = urllib.request.Request(url, headers=HEADERS)
        with urllib.request.urlopen(req) as response, open(dest_path, 'wb') as out_file:
            block_size = 1024 * 1024
            while True:
                buffer = response.read(block_size)
                if not buffer:
                    break
                out_file.write(buffer)
        return True
    except Exception as e:
        if os.path.exists(dest_path):
            os.remove(dest_path)
        return False

def main():
    print("=== SCRIPT 02: DESCARGA DE MICRODATOS HISTÓRICOS DANE EDUC (2014-2019) ===")
    for year, cat_id in CATALOG_IDS.items():
        print(f"Procesando Año {year} (Catalog ID {cat_id})...")
        year_dir = os.path.join(HISTORICO_DIR, str(year))
        os.makedirs(year_dir, exist_ok=True)
        links = obtener_enlaces_descarga(cat_id)
        for idx, link in enumerate(links, 1):
            target_zip = os.path.join(year_dir, f"educ_{year}_file_{idx}.zip")
            if not os.path.exists(target_zip):
                descargar_archivo(link, target_zip)
    print(f"--> Descarga histórica completada en: {HISTORICO_DIR}")

if __name__ == "__main__":
    main()
