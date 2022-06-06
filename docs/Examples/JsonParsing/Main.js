import * as Maybe from '../Data.Maybe/index.js'
export const parseJson = (str) => {
    try {
        return new Maybe.Just(JSON.parse(str))
    } catch (error) {
        return new Maybe.Nothing()
    }
}

export const getName = (obj) => {
    if (typeof obj.name == 'string') {
        return new Maybe.Just(obj.name)
    } else {
        return new Maybe.Nothing()
    }
}
