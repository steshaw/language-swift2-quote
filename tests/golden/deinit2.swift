class Player {
    var coinsInPurse: Int
    init(coins: Int) {
        coinsInPurse = vendCoins(coins)
    }
    func winCoins(coins: Int) {
        coinsInPurse += vendCoins(coins)
    }
    deinit {
        receiveCoins(coinsInPurse)
    }
}
