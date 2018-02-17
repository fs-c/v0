import React, { Component } from 'react'

import { Markdown } from './utils'

class PersonCard extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.person

    this.state = { hover: false }
  }

  // Very ugly solution, but it bypasses some potential bugs.
  mouseIn = () => this.setState({ hover: true })
  mouseOut = () => this.setState({ hover: false })
  
  handleClick = () => this.props.handleClick(this.person)

  render() {
    return (
      <div
        style={{cursor: "pointer"}}
        onClick={this.handleClick}
        className={`card ${this.state.hover ? 'border-primary' : ''}`}
        onMouseEnter={this.mouseIn} onMouseLeave={this.mouseOut}>

        <div className="card-body">
          <h4>{this.person.name}</h4>
          
          {
            this.person.notes && <Markdown
              text={ this.person.notes.length > 128 
                ? this.person.notes.slice(0, 127) + '...'
                : this.person.notes
              }
            />
          }
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