import { FictionsService } from './services/fictions';

export class RoyalRoadAPI {
  public readonly fictions: FictionsService;

  constructor() {
    this.fictions = new FictionsService();
  }
}