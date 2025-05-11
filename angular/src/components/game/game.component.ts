import { GameService } from '@/services/game.service';
import { Component } from '@angular/core';
import { DistrictComponent } from '@/components/district/district.component';
import { DISTRICT_NAMES } from '@/core/districts';

@Component({
  selector: 'app-game',
  standalone: true,
  templateUrl: './game.component.html',
  imports: [DistrictComponent],
})
export class GameComponent {
  constructor(private gameService: GameService) {}

  districts = DISTRICT_NAMES;
}
