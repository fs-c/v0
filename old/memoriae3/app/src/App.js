import request from 'request-promise'

import React, { Component } from 'react'
import './App.css'

import AddForm from './AddForm'
import SearchBar from './SearchBar'
import PersonsList from './PersonsList'
import PersonModal from './PersonModal'

const dev = process.env.NODE_ENV === 'development'

class App extends Component {
  constructor() {
    super()

    this.state = { persons: [  ], shown: [  ], open: null }

    this.updatePersons()
  }

  addPerson = p => {
    request({
      method: 'POST',
      uri: dev ? 'http://localhost:8080/api/persons' : 'https://fsoc.space/memoriae/api/persons',
      body: p,
      json: true
    }).then(() => {
      console.log('added person')
      setTimeout(5 * 1000, this.updatePersons)
    }).catch(console.error)
  }

  removePerson = p => {
    request({
      method: 'GET',
      uri: dev ? 'http://localhost:8080/api/remove/' + p._id : 'https://fsoc.space/memoriae/api/remove/' + p._id,
      json: true
    }).then(() => {
      console.log('removed person')
      this.updatePersons()
      this.setState({ open: false })
    }).catch(console.error)
  }

  togglePerson = p => {
    if (this.state.open) 
      return this.setState({ open: false })

    this.setState({ open: p._id })
  }
  
  updatePersons = () => {
    request({
      method: 'GET',
      uri: dev ? 'http://localhost:8080/api/persons' : 'https://fsoc.space/memoriae/api/persons',      
      json: true
    }).then(persons => {
      console.log('got persons')

      this.setState({
        persons,
        shown: persons.sort((a, b) => a.name.charCodeAt(0) - b.name.charCodeAt(0))
      })
    }).catch(console.error)
  }

  applySearch = s => {    
    this.setState({
      shown: this.state.persons.map(e => e.name.toLowerCase().indexOf(s) !== -1 ? e : false).filter(e => e)
    })
  }

  render() {
    return (
      <div className="App container">
        <div className="col-12">
          <SearchBar handleChange={this.applySearch} />
        </div>

        <div className="card-columns">
          <div className="card">
            <div className="card-body">
              <AddForm handleAdd={this.addPerson} />
            </div>
          </div>

          <PersonsList persons={this.state.shown} handleClick={this.togglePerson} />

          {
            this.state.persons.map(p => 
              <PersonModal
                person={p}
                key={p._id}
                handleClose={this.togglePerson}
                isOpen={this.state.open === p._id}
                
                removePerson={this.removePerson}
              />
            )
          }
        </div>
      </div>
    )
  }
}

export default App
