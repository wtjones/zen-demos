class IdGenerator():
    last_id = 0

    @staticmethod
    def generate_id():
        IdGenerator.last_id += 1
        return IdGenerator.last_id
