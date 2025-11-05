import socket
import threading
import time


class NetworkClient:
    """
    Cliente TCP para comunicarse con el servidor Erlang.
    Usa un hilo separado para escuchar mensajes y mantener la UI libre.
    """
    def __init__(self, server_ip="127.0.0.1", port=4000):
        self.server_ip = server_ip
        self.port = port
        self.socket = None
        self.connected = False
        self.listener_thread = None
        self.message_queue = []
        self.last_message = ""
        self._stop_flag = False

    # --- Conexión ---
    def connect(self):
        """Intenta conectarse al servidor TCP."""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((self.server_ip, self.port))
            self.connected = True
            print(f"✅ Conectado al servidor {self.server_ip}:{self.port}")

            # Inicia hilo de escucha
            self.listener_thread = threading.Thread(target=self.listen_server, daemon=True)
            self.listener_thread.start()
        except Exception as e:
            print(f"❌ Error al conectar: {e}")
            self.connected = False

    # --- Escucha ---
    def listen_server(self):
        """Hilo que recibe mensajes del servidor de manera continua."""
        try:
            while not self._stop_flag:
                data = self.socket.recv(1024)
                if not data:
                    break
                message = data.decode("utf-8", errors="ignore").strip()
                if message:
                    self.last_message = message
                    self.message_queue.append(message)
                    print(f"📨 Servidor -> {message}")
        except Exception as e:
            print(f"⚠️ Error en hilo de escucha: {e}")
        finally:
            self.connected = False
            print("🔴 Conexión cerrada con el servidor.")

    # --- Envío ---
    def send(self, message):
        """Envía texto al servidor."""
        if self.connected and self.socket:
            try:
                if not message.endswith("\n"):
                    message += "\n"
                self.socket.sendall(message.encode("utf-8"))
                print(f"➡️ Enviado al servidor: {message.strip()}")
            except Exception as e:
                print(f"❌ Error al enviar: {e}")
                self.connected = False

    # --- Recepción ---
    def get_messages(self):
        """Devuelve y limpia la cola de mensajes recibidos."""
        messages = self.message_queue[:]
        self.message_queue.clear()
        return messages

    # --- Cierre ---
    def close_connection(self):
        """Cierra la conexión TCP y detiene el hilo."""
        self._stop_flag = True
        if self.socket:
            try:
                self.socket.close()
            except:
                pass
        self.connected = False
        print("🧩 Conexión TCP finalizada por el cliente.")
