def checkPhase(block_number, halving, mining_pool, mining_rate, trade_flag):
    """
    """
    # Stop if nothing in pool
    if mining_pool <= 0:
        return False, mining_rate
    # Handle halving
    if block_number % halving == 0 and block_number != 0:
        if mining_rate > 1:
            mining_rate = mining_rate / 2
        return True, mining_rate
    return True, mining_rate