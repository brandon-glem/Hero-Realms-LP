import arcade
import json
import socket
import threading

import os
import random

from card import Card

# ============================
# CONFIGURACION
# ============================
SCREEN_WIDTH = 1280
SCREEN_HEIGHT = 720
SCREEN_TITLE = "Hero Realms LP - Demo TCP"

# ============================
# CLIENTE DE RED
# ============================
class NetworkClient:
    def __init__(self, host, port=4000):
        self.host = host
        self.port = port
        self.sock = None
        self.connected = False
        self.last_message = ""
        self.on_message = None  # callback

    def connect(self):
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((self.host, self.port))
            self.connected = True
            print(f"✅ Conectado al servidor {self.host}:{self.port}")
            threading.Thread(target=self._listen, daemon=True).start()
        except Exception as e:
            print("❌ ERROR AL CONECTAR:", e)

    def send(self, data):
        if self.connected:
            try:
                self.sock.sendall(data.encode())
            except Exception as e:
                print("Error al enviar:", e)

    def _listen(self):
        while self.connected:
            try:
                data = self.sock.recv(1024)
                if not data:
                    break
                msg = data.decode("utf-8").strip()
                if self.on_message:
                    self.on_message(msg)
            except Exception:
                break
        self.connected = False
        print("🔌 Conexión cerrada con el servidor.")

# ============================
# CLIENTE GRAFICO
# ============================
class HeroRealmsClient(arcade.Window):
    def __init__(self, server_ip):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_TITLE, update_rate=1/60)
        arcade.set_background_color(arcade.color.AMAZON)

        # Red
        self.network = NetworkClient(server_ip)
        self.network.on_message = self._process_erlang_message
        self.network.connect()

        # Estado del juego
        self.current_turn = "Esperando jugadores"
        self.player_role = None

        # Fondo
        self.background_list = arcade.SpriteList()
        bg = arcade.Sprite(center_x=SCREEN_WIDTH // 2, center_y=SCREEN_HEIGHT // 2)
        bg.texture = arcade.load_texture("assets/fondo.png")
        bg.width = SCREEN_WIDTH
        bg.height = SCREEN_HEIGHT
        self.background_list.append(bg)

        # Mano de cartas
        self.player_hand = arcade.SpriteList()
        self._create_sample_hand()

        # Mercado
        self.market = arcade.SpriteList()
        self._create_market()

        # Boton "Fin de Turno"
        self.end_turn_button_list = arcade.SpriteList()
        button = arcade.Sprite("assets/boton_turno.png", scale=0.08,  # ← pequeño
                               center_x=SCREEN_WIDTH - 80, center_y=SCREEN_HEIGHT - 60)
        self.end_turn_button_list.append(button)
        self.end_turn_button = button

    def _create_sample_hand(self):
        """Crea cartas de ejemplo en la parte inferior."""
        card_paths = [
            "assets/primarygold1.png",
            "assets/g1.png",
            "assets/b1.png",
            "assets/primarydagger1.png",
            "assets/primaryplayer1.png"
        ]
        start_x = 400
        y = 150
        for i, path in enumerate(card_paths):
            card = Card(path, scale=0.3, card_id=f"C{i+1}")
            card.update_position(start_x + i * 120, y)
            card.is_clickable = True
            self.player_hand.append(card)
    
    def _create_market(self):
        ### Aqui creamos el mercado, entiendo que se crean de forma aleatoria.
        assets_dir = os.path.join(os.path.dirname(__file__), "assets")
        ### Aqui agregar los excluidos para evitar que aparezcan elementos "No vendibles"
        exclude = {"fondo.png", "boton_turno.png"}
        gema = "primarygoldattack.png"

        start_x = 400
        y = 450

        candidates = [f for f in os.listdir(assets_dir)
                      if f.lower().endswith((".png", ".jpg", ".jpeg"))
                      and f not in exclude
                      and f != gema]
        
        ### Elegimos hasta 4 randoms unicos para el mercado (slots del 2 al 5)
        elegidos = random.sample(candidates, min(4, len(candidates))) if candidates else []

        cartas = [gema] + elegidos
        
        for i, fname in enumerate(cartas):
            path = os.path.join("assets", fname)  # le pasamos el path relativo de assets
            id_base = os.path.splitext(fname)[0]
            card_id = f"M{i+1}_{id_base}"
            card = Card(path, scale=0.3, card_id=card_id)
            card.update_position(start_x + i * 120, y)
            card.is_clickable = True
            self.market.append(card)

    # ============================
    # EVENTOS GRAFICOS
    # ============================

    def on_draw(self):
        self.clear()

        ### Todo lo dibujado va aqui
        self.background_list.draw()
        self.player_hand.draw()
        self.market.draw()

        # Mostrar turno y conexion
        arcade.draw_text(f"Conexión: {'Conectado' if self.network.connected else 'Desconectado'}",
                         20, SCREEN_HEIGHT - 40, arcade.color.WHITE, 16)

        color_turno = arcade.color.GREEN if self.current_turn == self.player_role else arcade.color.GRAY
        turno_text = "Tu turno" if self.current_turn == self.player_role else "Esperando..."
        arcade.draw_text(f"{turno_text} ({self.current_turn})", 20, SCREEN_HEIGHT - 70, color_turno, 18)

        # Dibujar boton si es tu turno
        if self.player_role == self.current_turn:
            self.end_turn_button_list.draw()

    ### MOUSE
    ### ANIMACION MOUSE HOVER
    def on_mouse_motion(self, x, y, dx, dy):
        for card in self.player_hand:
            if card.is_clickable:
                if card.collides_with_point((x, y)):
                    card.on_hover()
                else:
                    card.on_unhover()

        for card in self.market:
            if card.is_clickable:
                if card.collides_with_point((x, y)):
                    card.on_hover()
                else:
                    card.on_unhover()

    ### MOUSE CLICK
    def on_mouse_press(self, x, y, button, modifiers):
        if self.player_role != self.current_turn:
            print("❌ No es tu turno.")
            return

        # Fin de turno
        if self.end_turn_button.collides_with_point((x, y)):
            print("➡️ Fin de turno enviado.")
            self.network.send("END_TURN")
            return

        # Clic en carta
        clicked = arcade.get_sprites_at_point((x, y), self.player_hand)
        if clicked:
            card = clicked[0]
            msg = json.dumps({"action": "play_card", "id": card.card_id})
            self.network.send(msg)
            print(f"🃏 {self.player_role} jugó carta {card.card_id}")

        # Click en mercado
        clicked_market = arcade.get_sprites_at_point((x, y), self.market)
        if clicked_market:
            card = clicked_market[0]
            msg = json.dumps({"action": "buy_card", "id": card.card_id})
            self.network.send(msg)
            print(f"🛒 {self.player_role} compró carta {card.card_id}")
            return

    # ============================
    # PROCESAMIENTO DE MENSAJES
    # ============================

    def _process_erlang_message(self, message):
        msg = message.strip()
        print(f"📨 Servidor -> {msg}")

        if msg.startswith("{"):
            try:
                data = json.loads(msg)
                if data.get("action") == "update":
                    turn = data.get("turn")
                    # Si aun no sabe quien soy, me asigna mi rol
                    if not self.player_role:
                        self.player_role = turn
                    self.current_turn = turn
            except Exception as e:
                print("Error al interpretar JSON:", e)


# ============================
# MAIN
# ============================

def main():
    server_ip = input("IP del servidor Erlang: ").strip()
    game = HeroRealmsClient(server_ip)
    arcade.run()

if __name__ == "__main__":
    main()
