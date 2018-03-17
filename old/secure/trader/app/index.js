const trades = [  ]

const TradesList = {
  view: () => m('div', { class: 'container' }, [
    m('div', { class: 'row' }, trades.map(trade => {
      m('div', { class: 'twelve columns trade' }, trade.id)
    }))
  ])
}

m.mount(document.body, TradesList)