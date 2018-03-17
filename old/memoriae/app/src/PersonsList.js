import React, { Component } from 'react'

class PersonCard extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.person

    this.state = { hover: false }
  }

  handleHover = () => this.setState({ hover: !this.state.hover })
  handleClick = () => this.props.handleClick(this.person)

  render() {
    return (
      <div
        style={{cursor: "pointer"}}
        onClick={this.handleClick}
        className={`card ${this.state.hover ? 'border-primary' : ''}`}
        onMouseEnter={this.handleHover} onMouseLeave={this.handleHover}>

        <div className="card-body">
          <h4>{this.person.name}</h4>
          
          <p className="card-text">{
            this.person.notes.length > 128 
            ? this.person.notes.slice(0, 127) + '...'
            : this.person.notes
          }</p>

        </div>
      </div>
    )
  }
}

export default class PersonsList extends Component {
  handleClick = person => this.props.handleClick(person)

  render() {
    return (
      <div>
        {this.props.persons.map(p =>
          <PersonCard person={p} key={p._id} handleClick={this.handleClick} />
        )}
      </div>
    )
  }
}