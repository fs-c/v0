import React, { Component } from 'react'
import Gun from 'gun/gun'

import './App.css'

import Sidebar from './Sidebar.js'
import Content from './Content.js'

class App extends Component {
  render() {
    return (
      <div className="App container">
        <nav class="navbar navbar-light">
          <a class="navbar-brand" href="/">memoriae</a>
        </nav>

        <main role="main" class="row">
          <div class="col-3">
            <Sidebar />
          </div>

          <div class="col">
            <Content />
          </div>
        </main>
      </div>
    )
  }
}

export default App
