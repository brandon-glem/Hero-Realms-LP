
HERO REALMS LP - DEMO TCP

📌 Descripción
--------------
Este proyecto implementa una versión básica del juego de cartas "Hero Realms"
conectando dos computadoras en red local. El backend (servidor) está escrito en
Erlang, y el frontend (interfaz gráfica) está desarrollado en Python con Arcade.

El objetivo del demo es mostrar:
- Comunicación TCP cliente-servidor entre dos PCs.
- Visualización del fondo y las cartas del jugador.
- Interacción básica: detección de hover y clic en cartas.

REQUISITOS DEL SISTEMA

🔹 En ambas computadoras:
- Python 3.9 o superior (recomendado Python 3.12)
- Librería Arcade instalada
- Librería Pillow (para cargar imágenes)
- Librería websocket-client (opcional, pero recomendada)
- Imágenes en carpeta "assets/"

🔹 Solo en la computadora que actuará como servidor:
- Erlang/OTP 24 o superior (recomendado 25 o 26)

INSTALACIÓN DE DEPENDENCIAS

1. Verifica tu versión de Python:
   > python --version

2. Instala las librerías necesarias:
   > pip install arcade pillow websocket-client

3. (Opcional) Verifica la instalación:
   > python -m arcade.version

   Si muestra algo como "Arcade 2.6.x" o "Arcade 3.x", está correcto.

4. (Servidor) Instala Erlang (si aún no lo tienes):
   - Descarga desde: https://www.erlang.org/downloads
   - Abre CMD o terminal y verifica con:
     > erl

EJECUCIÓN DEL SERVIDOR ERLANG

1. Abre una terminal (CMD o PowerShell, NO WSL si deseas conexión entre PCs).
2. Entra al directorio del backend:
   > cd ruta\del\proyecto\backend

3. Inicia Erlang:
   > erl

4. Compila y arranca el servidor:
   1> c(game_server).
   2> game_server:start().

5. Verás:
   "Servidor iniciado en puerto 4000"

6. Mantén esa ventana abierta durante la demo.

EJECUCIÓN DEL CLIENTE (FRONTEND)

1. Copia la carpeta `frontend/` a ambas computadoras.
2. En cada PC, abre una terminal y ejecuta:
   > cd ruta\del\proyecto\frontend

3. Ejecuta el cliente:
   > python client.py

4. Cuando te pida:
   "IP del servidor Erlang: "
   escribe la dirección IPv4 de la máquina que ejecuta el servidor, por ejemplo:
   > 192.168.1.8

5. Si todo está correcto verás:
   ✅ Conectado al servidor
   📨 Servidor -> Bienvenido al servidor Hero Realms!

6. En el servidor, verás:
   Cliente conectado

CONTROLES Y FUNCIONALIDAD

- Pasa el mouse sobre las cartas → se elevan (efecto hover).
- Clic en una carta → envía mensaje al servidor ("play_card").

NOTAS ADICIONALES

- Ambas computadoras deben estar en la misma red local Wi-Fi o LAN.
- El firewall de Windows debe permitir conexiones entrantes al puerto 4000.
- Puedes cambiar la IP del servidor directamente en client.py si quieres fijarla.
Frontend: Python + Arcade
