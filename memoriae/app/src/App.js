import React, { Component } from 'react'
import './App.css'

import Gun from 'gun/gun'

import AddForm from './AddForm.js'
import PersonsList from './PersonsList.js'

class App extends Component {
  constructor() {
    super()

    this.gun = Gun('http://localhost:8080/gun')
  }

  addPerson = p => {
    this.gun.get('persons').set(p)
  }

  render() {
    return (
      <div className="App container">
        <div className="card-columns">
          <div className="card">
            <div className="card-body">
              <AddForm handleAdd={this.addPerson} />
            </div>
          </div>

          <PersonsList gun={this.gun}/>
        </div>
      </div>
    )
  }
}

export default App
