import arcade

class Card(arcade.Sprite):
    """
    Representa una carta en la pantalla. Hereda de arcade.Sprite.
    """
    def __init__(self, image_path, scale=0.5, card_id=None):
        super().__init__(image_path, scale)
        self.card_id = card_id # ID único para enviar al servidor
        self.is_clickable = False
        self.original_y = 0

    def on_hover(self):
        """ Mueve la carta ligeramente hacia arriba al pasar el ratón. """
        if self.is_clickable:
            self.center_y = self.original_y + 20

    def on_unhover(self):
        """ Devuelve la carta a su posición original. """
        self.center_y = self.original_y
        
    def update_position(self, x, y):
        """ Establece la posición y guarda la posición base. """
        self.center_x = x
        self.center_y = y
        self.original_y = y