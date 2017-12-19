import React, { Component } from 'react'

class PersonCard extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.person
  }

  render() {
    return (
      <div className="card">
        <div className="card-body">
          <h4 className="card-title">{this.person.name}</h4>
          <p className="card-text">{this.person.notes}</p>
        </div>
      </div>
    )
  }
}

export default class PersonsList extends Component {
  render() {
    return (
      <div>
        {this.props.persons.map(p => <PersonCard person={p} key={p._id} /> )}
      </div>
    )
  }
}