package com.github.matsubara0507

fun main(args: Array<String>) {
    TodoAPI("http://localhost:8000").getTodos({ _, _, _ -> Unit })
}
