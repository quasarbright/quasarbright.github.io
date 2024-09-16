class JsonMap {
    constructor() {
        this.map = new Map()
    }

    get(k) {
        return this.map.get(JSON.stringify(k))
    }

    has(k) {
        return this.map.has(JSON.stringify(k))
    }

    set(k, v) {
        return this.map.set(JSON.stringify(k), v)
    }

    *[Symbol.iterator]() {
        for (const [ks, v] of this.map) {
            yield [JSON.parse(ks), v]
        }
    }

    copy() {
        const jm = new JsonMap()
        jm.map = new Map(jm.map)
        return jm
    }
}