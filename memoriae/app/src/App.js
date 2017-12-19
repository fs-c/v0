import axios from 'axios'

import React, { Component } from 'react'
import './App.css'

import AddForm from './AddForm.js'
import PersonsList from './PersonsList.js'

class App extends Component {
  constructor() {
    super()
  }

  addPerson = p => {
    console.log(p)

    axios.post('http://localhost:8080/api/persons', p)
      .then(console.log)
      .catch(console.error)
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

          <PersonsList />
        </div>
      </div>
    )
  }
}

export default App
