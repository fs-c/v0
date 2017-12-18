import React, { Component } from 'react'

const format = items => Object.keys(items)
  .map(key => ({ key, val: items[key] }))
  .filter(t => Boolean(t.val) && t.key !== '_')

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
  constructor() {
    super()

    this.state = { persons: [] }
  }

  componentWillMount = () => {
  }

  render() {
    return (
      <div>
      </div>
    )
  }
}