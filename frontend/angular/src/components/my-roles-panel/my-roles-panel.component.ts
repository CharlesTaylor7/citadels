import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RoleName } from '@/core/roles';
// May need a RoleCardComponent similar to DistrictComponent later

@Component({
  selector: 'app-my-roles-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './my-roles-panel.component.html',
})
export class MyRolesPanelComponent {
  roles = input<RoleName[]>();
}
