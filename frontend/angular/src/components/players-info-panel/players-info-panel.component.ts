import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-players-info-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './players-info-panel.component.html',
})
export class PlayersInfoPanelComponent {
  playersInfo = input<any[]>(); // Corresponds to players.* in Rust template
}
