import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { GameService } from '@/services/game.service';

// Import new child components
import { RolesPanelComponent } from '../roles-panel/roles-panel.component';
import { MyStashPanelComponent } from '../my-stash-panel/my-stash-panel.component';
import { PlayerCityPanelComponent } from '../player-city-panel/player-city-panel.component';
import { GameMenuPanelComponent } from '../game-menu-panel/game-menu-panel.component';
import { HandPanelComponent } from '../hand-panel/hand-panel.component';
import { MyRolesPanelComponent } from '../my-roles-panel/my-roles-panel.component';
import { PlayersInfoPanelComponent } from '../players-info-panel/players-info-panel.component';
import { MiscInfoPanelComponent } from '../misc-info-panel/misc-info-panel.component';
import { GameEndModalComponent } from '../game-end-modal/game-end-modal.component';
import { PauseModalComponent } from '../pause-modal/pause-modal.component';

@Component({
  selector: 'app-game',
  standalone: true,
  templateUrl: './game.component.html',
  imports: [
    CommonModule,
    RolesPanelComponent,
    MyStashPanelComponent,
    PlayerCityPanelComponent,
    GameMenuPanelComponent,
    HandPanelComponent,
    MyRolesPanelComponent,
    PlayersInfoPanelComponent,
    MiscInfoPanelComponent,
    GameEndModalComponent,
    PauseModalComponent,
  ],
})
export class GameComponent {
  // Assuming GameService exposes a signal for the entire game state.
  // The structure of this gameState object would mirror the data needed by the template,
  // similar to the Rust GameTemplate struct.
  // For example:
  // gameState = signal<{
  //   characters?: any[];
  //   myPlayer?: any;
  //   city?: any;
  //   menu?: any;
  //   context?: any;
  //   playersInfo?: any[];
  //   miscInfo?: any;
  //   gameEnd?: any;
  //   isPaused?: boolean; // For pause modal
  //   ringBell?: boolean; // For audio notification
  // } | null>(null);

  // For now, let's use the gameService directly in the template for simplicity,
  // assuming it has a gameStateSignal.
  constructor(public gameService: GameService) {
    // Example of how gameState might be populated if GameService provides it:
    // this.gameState = this.gameService.gameStateSignal;
  }

  // Placeholder for handling actions, e.g., from the menu
  handleAction(actionDetails: any) {
    console.log('Action triggered:', actionDetails);
    // This would typically call a method on GameService
    // this.gameService.performAction(actionDetails);
  }

  // Placeholder for audio bell logic
  // effect(() => {
  //   if (this.gameState()?.ringBell) {
  //     const audio = document.getElementById('bell-audio') as HTMLAudioElement;
  //     audio?.play();
  //   }
  // });
}
