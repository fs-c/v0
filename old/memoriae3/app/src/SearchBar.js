import React, { Component } from 'react'

const inputStyle = {
  width: '100%',
  border: '0px solid white',
  marginBottom: '1em',
  outline: 'none'
}

export default class SearchBar extends Component {
  constructor(props) {
    super(props)

    this.state = { value: '' }
  }

  handleChange = e => {
    this.setState({
      value: e.target.value
    })

    this.props.handleChange(e.target.value)
  }

  render() {
    return (
      <input
        name="search"
        placeholder="Start typing..."
        value={this.state.value}
        style={inputStyle}
        onChange={this.handleChange}
        autoFocus
      />
    )
  }
}