import arcade
import json
from network_client import NetworkClient
from card import Card

# --- CONSTANTES ---
SCREEN_WIDTH = 1200
SCREEN_HEIGHT = 800
CARD_SCALE = 0.5
CARD_WIDTH = 140 * CARD_SCALE

class HeroRealmsClient(arcade.Window):
    """
    Clase principal que maneja la interfaz gráfica y la lógica del cliente.
    """
    def __init__(self):
        super().__init__(SCREEN_WIDTH, SCREEN_HEIGHT, "Hero Realms LP")
        arcade.set_background_color(arcade.color.AMAZON)
        
        # Conexión al servidor de Erlang
        self.network = NetworkClient()
        
        # Estado del juego
        self.current_turn = "Esperando Jugadores..."
        
        # Elementos gráficos
        self.player_hand = arcade.SpriteList()
        self.card_being_hovered = None

    def setup(self):
        """ Inicializa la conexión y carga los recursos. """
        self.network.start_connection()
        
        # Simulación de cartas (necesitas reemplazar esto con tus assets)
        # Asegúrate de tener una carpeta 'assets' con imágenes.
        try:
            self.load_sample_hand()
        except FileNotFoundError:
             print("ADVERTENCIA: No se pudieron cargar las imágenes de las cartas. Asegúrate de tener la carpeta 'assets'.")
             
    def load_sample_hand(self):
        """ Carga una mano de prueba (ejemplo). """
        # Asegúrate de que esta ruta sea válida
        IMAGE_PATH = "assets/carta_ejemplo.png" 
        
        for i in range(5):
            # Crea una carta (usa un ID de prueba)
            card = Card(IMAGE_PATH, CARD_SCALE, card_id=f"C{i+1}")
            
            # Posición de la carta en la mano del jugador
            x = (i * (CARD_WIDTH + 10)) + 300 
            y = 150
            card.update_position(x, y)
            card.is_clickable = True
            
            self.player_hand.append(card)

    def on_draw(self):
        # Llama a clear() para limpiar la ventana en cada frame
        self.clear() 

        # Dibuja el texto que tienes definido (Jugador, vida, etc.)
        self.label.draw()
        self.vida_label.draw()
        
        # Dibujar la mano del jugador
        self.player_hand.draw()

        # Mostrar estado y turno actual
        arcade.draw_text(f"Conexión: {self.network.status} | Turno: {self.current_turn}", 
                         10, SCREEN_HEIGHT - 30, arcade.color.WHITE, 18)
        
    def on_update(self, delta_time):
        """ Lógica de actualización del juego. """
        
        # 1. Procesar mensajes de Erlang
        for message in self.network.get_messages():
            self._process_erlang_message(message)
            
        # 2. Actualizar el estado de la mano (hover)
        if self.card_being_hovered:
            self.card_being_hovered.on_hover()
        
    def _process_erlang_message(self, message):
        """ Decodifica y aplica los cambios enviados por el servidor de Erlang. """
        try:
            # Tu handler de Erlang envía: {"action":"update","turn":"PlayerA"}
            data = json.loads(message)
            
            if data.get("action") == "update" and data.get("turn"):
                self.current_turn = data["turn"]
                print(f"Turno actualizado a: {self.current_turn}")
                
            # Aquí iría más lógica para actualizar el tablero, robar cartas, etc.
                
        except json.JSONDecodeError:
            print(f"Error al decodificar JSON: {message}")
            
    def on_mouse_motion(self, x: float, y: float, dx: float, dy: float):
        """ Manejo del movimiento del ratón para efectos visuales (hover). """
        if self.card_being_hovered:
            self.card_being_hovered.on_unhover()
            self.card_being_hovered = None

        # Verificar qué carta está siendo apuntada
        hit_sprites = arcade.get_sprites_at_point((x, y), self.player_hand)
        if hit_sprites:
            self.card_being_hovered = hit_sprites[0]

    def on_mouse_press(self, x, y, button, modifiers):
        """ Manejo de clics (ej. jugar una carta o pasar el turno). """
        
        # 1. Detectar si se hizo clic en una carta jugable
        clicked_sprites = arcade.get_sprites_at_point((x, y), self.player_hand)
        if clicked_sprites and clicked_sprites[0].is_clickable:
            card = clicked_sprites[0]
            print(f"Carta {card.card_id} clicada. Enviando al servidor...")
            
            # **Asegúrate de que el formato de envío coincida con lo que espera Erlang**
            # (El Matchmaker de Erlang solo acepta END_TURN por ahora)
            # self.network.send_message(f'{{"action":"play_card", "id":"{card.card_id}"}}')
            
        # 2. Lógica del botón "Fin de Turno" (simulación)
        if x > 10 and x < 200 and y > 10 and y < 50:
             self.network.send_message("END_TURN")
             print("Solicitud de fin de turno enviada a Erlang.")
             
    def on_close(self):
        """ Se llama al cerrar la ventana. """
        self.network.close_connection()
        super().on_close()

if __name__ == "__main__":
    game = HeroRealmsClient()
    game.setup()
    arcade.run()