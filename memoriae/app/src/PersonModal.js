import React, { Component } from 'react'

import Modal from 'react-modal'
import Icon from './Icon'

const modalStyles = {
  content : {
    top                   : '50%',
    left                  : '50%',
    right                 : 'auto',
    bottom                : 'auto',
    marginRight           : '-50%',
    transform             : 'translate(-50%, -50%)'
  }
}

export default class PersonModal extends Component {
  constructor(props) {
    super(props)

    this.person = this.props.person
  }

  componentDidMount = () => {}

  render() {
    return (
      <Modal
        ariaHideApp={false}
        className="col-sm-12 col-md-3"
        style={modalStyles}
        isOpen={this.props.isOpen}
      >

        <div className="card">
          <div className="card-header">
            <Icon icon="cross" dim={10} className="float-right" />
          </div>

          <div className="card-body">
            <h3>{this.person.name}</h3>

            <div className="row">
              <div className="col-md-6 col-12"><b>Phone: </b>{this.person.phone}</div>
              <div className="col-md-6 col-12"><b>Email: </b>{this.person.email}</div>
              <div className="col-md-6 col-12"><b>Born: </b>{this.person.birth}</div>            
            </div>

            <hr />

            <p>{this.person.notes}</p>
          </div>
        </div>
      </Modal>
    )
  }
}