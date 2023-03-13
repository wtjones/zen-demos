import concurrent.futures
import time

couple_ods = []
futures = []


def do_thing(count: int, ex):
    print(f'sleep {count}')

    time.sleep(count)
    if count == 3:

        futures.append(ex.submit(do_thing, 5, ex))
    return f'return: {count}'


def main():

    numbers = [1, 2, 3, 4]

    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(do_thing, number, executor)
                   for number in numbers]

        for f in concurrent.futures.as_completed(futures):
            print(f.result())


if __name__ == '__main__':
    main()
