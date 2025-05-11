import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-pause-modal',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './pause-modal.component.html',
})
export class PauseModalComponent {
  isPaused = input<boolean>(false);
  // Theme dropdown logic would be added here or as a separate component
}
