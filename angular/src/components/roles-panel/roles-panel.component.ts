import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-roles-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './roles-panel.component.html',
})
export class RolesPanelComponent {
  characters = input<any[]>(); // Corresponds to game.characters.0 in Rust template
}
