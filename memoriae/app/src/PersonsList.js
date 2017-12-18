import React, { Component } from 'react'

class PersonCard extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.p
  }

  render() {
    return (
      <div class="card">
        <div class="card-body">
          <h4 class="card-title">{this.person.name}</h4>
          <p class="card-text">{this.person.notes}</p>
        </div>
      </div>
    )
  }
}

export default class PersonsList extends Component {
  constructor({ gun }) {
    super()

    this.gun = gun.get('persons')

    this.state = { persons: [] }
  }

  componentWillMount = () => {
    this.gun.on(p => {
      console.log(p)
      this.setState({ persons: p })
    })
  }

  render() {
    return (
      <div>
      </div>
    )
  }
}