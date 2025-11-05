import threading
import websocket
import json
import time

class NetworkClient:
    """
    Gestiona la conexión WebSocket con el servidor de Erlang en un hilo separado.
    """
    def __init__(self, url="ws://localhost:8080/ws"):
        self.url = url
        self.ws = None
        self.thread = None
        self.is_running = True
        self.message_queue = []
        self.status = "Desconectado"

    def start_connection(self):
        """ Inicia la conexión en un hilo separado. """
        self.thread = threading.Thread(target=self._run_ws)
        self.thread.daemon = True
        self.thread.start()
        self.status = "Conectando..."

    def _run_ws(self):
        """ El bucle principal del WebSocket (se ejecuta en el hilo). """
        while self.is_running:
            try:
                # Conexión al servidor de Erlang (puerto 8080, ruta /ws)
                self.ws = websocket.WebSocketApp(
                    self.url,
                    on_open=self._on_open,
                    on_message=self._on_message,
                    on_error=self._on_error,
                    on_close=self._on_close
                )
                self.ws.run_forever(ping_interval=30, ping_timeout=10)
            except Exception as e:
                print(f"Error al intentar conectar: {e}")
                self.status = "Reconectando..."
                time.sleep(5) # Espera antes de reintentar

    def _on_open(self, ws):
        """ Se llama cuando la conexión se abre correctamente. """
        print("Conexión establecida con el servidor de Erlang.")
        self.status = "Conectado"
        
    def _on_message(self, ws, message):
        """ Se llama cuando se recibe un mensaje de Erlang. """
        # Los mensajes se ponen en una cola para que el bucle de Arcade los lea
        self.message_queue.append(message)

    def _on_error(self, ws, error):
        """ Se llama cuando hay un error. """
        print(f"Error de WS: {error}")
        self.status = f"Error: {error}"

    def _on_close(self, ws, close_status_code, close_msg):
        """ Se llama cuando la conexión se cierra. """
        print(f"Conexión WS cerrada: {close_status_code} - {close_msg}")
        self.status = "Desconectado"
        
    def send_message(self, message):
        """ Envía un mensaje al servidor de Erlang. """
        if self.ws and self.ws.sock and self.ws.sock.connected:
            self.ws.send(message)
            return True
        return False

    def get_messages(self):
        """ Devuelve la cola de mensajes recibidos y la limpia. """
        messages = self.message_queue
        self.message_queue = []
        return messages

    def close_connection(self):
        """ Cierra la conexión y detiene el hilo. """
        self.is_running = False
        if self.ws:
            self.ws.close()
        if self.thread:
            self.thread.join(timeout=1)