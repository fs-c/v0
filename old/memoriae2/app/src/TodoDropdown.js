import React, { Component } from 'react'

export default class TodoDropdown extends Component {
  render() {
    return (
      <ul class="list-group lust-group-flush list-group-item-action">
        <a href="#" class="list-group-item list-group-item-action">Finish Facebook auth implementation</a>
        <a href="#" class="list-group-item list-group-item-action">Research osTicket server migration</a>
      </ul>
    )
  }
}