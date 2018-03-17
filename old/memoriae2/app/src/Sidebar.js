import React, { Component } from 'react'

import TodoDropdown from './TodoDropdown.js'

export default class Sidebar extends Component {
  render() {
    return (
      <div class="card">
        <ul class="list-group list-group-flush">
          <a href="#" class="list-group-item list-group-item-action">People</a>
        </ul>

        <div class="card-space"></div>

        <ul class="list-group list-group-flush">                
          <a href="#list-todo-collapse" data-toggle="collapse" class="list-group-item d-flex justify-content-between align-items-center list-group-item-action">
            To-Do
            <span class="badge badge-primary badge-pill">2</span>                
          </a>

          <div class="collapse" id="list-todo-collapse">
            <TodoDropdown />
          </div>
        </ul>

        <div class="card-space"></div>            

        <ul class="list-group list-group-flush">                
          <a href="#" class="list-group-item list-group-item-action">Notes</a>
        </ul>
      </div>
    )
  }
}