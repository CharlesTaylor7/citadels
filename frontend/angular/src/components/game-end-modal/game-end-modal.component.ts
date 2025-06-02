import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-game-end-modal',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './game-end-modal.component.html',
})
export class GameEndModalComponent {
  gameEndData = input<any>(); // Corresponds to end.* in Rust template
}
