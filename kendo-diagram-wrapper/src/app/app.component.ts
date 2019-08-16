import { Component, ViewChild, ElementRef, AfterViewInit } from '@angular/core';

declare var kendo: any;

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'kendo-diagram-wrapper';

  constructor() {
    if (window !== undefined) {
      (window as any).$ = kendo.jQuery;
    }
  }

  /*
  @ViewChild('h1Element') el: ElementRef;
  @ViewChild('datePicker') datePickerEl: ElementRef;

  selectedDate: Date = new Date();

   ngAfterViewInit() {
       // Using a template reference variable
       kendo.jQuery(this.el.nativeElement).css('color', 'red');

       // Using a template reference variable
       kendo.jQuery(this.datePickerEl.nativeElement).kendoDatePicker({
           change: (e) => {
               this.selectedDate = e.sender.value();
           }
       });
   }*/
}
