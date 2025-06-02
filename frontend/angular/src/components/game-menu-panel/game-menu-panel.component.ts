import { Component, input, output } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-game-menu-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './game-menu-panel.component.html',
})
export class GameMenuPanelComponent {
  menuData = input<any>(); // Corresponds to menu.* in Rust template
  gameContext = input<any>(); // Corresponds to context.* for actions
  action = output<any>(); // Emits action details

  // Placeholder for sending an action
  performAction(actionDetails: any) {
    this.action.emit(actionDetails);
  }
}
