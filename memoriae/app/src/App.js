import request from 'request-promise'

import React, { Component } from 'react'
import './App.css'

import AddForm from './AddForm.js'
import PersonsList from './PersonsList.js'

class App extends Component {
  constructor() {
    super()

    this.state = { persons: [  ] }

    this.updatePersons()
  }

  addPerson = p => {
    request({
      method: 'POST',
      uri: 'http://localhost:8080/api/persons',
      body: p,
      json: true
    }).then(() => {
      console.log('added person')
      this.updatePersons()
    }).catch(console.error)
  }

  getPersons = () => {
    return request({
      method: 'GET',
      uri: 'http://localhost:8080/api/persons',
      json: true
    })
  }

  updatePersons = () => {
    this.getPersons().then(persons => this.setState({ persons }))
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

          <PersonsList persons={this.state.persons} />
        </div>
      </div>
    )
  }
}

export default App
