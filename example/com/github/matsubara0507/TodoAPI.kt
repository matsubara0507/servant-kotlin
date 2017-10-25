package com.github.matsubara0507

import com.github.kittinunf.fuel.Fuel
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.fuel.core.FuelManager
import com.github.kittinunf.fuel.core.Request
import com.github.kittinunf.fuel.core.Response
import com.github.kittinunf.fuel.gson.responseObject
import com.github.kittinunf.result.Result
import com.google.gson.Gson

class TodoAPI(private val baseURL: String) {

    init {
        FuelManager.instance.apply {
            basePath = baseURL
            baseHeaders = mapOf("Content-Type" to "application/json", "Device" to "Android")
        }
    }

    data class Todo(val todoId: Int, val title: String, val done: Boolean)

    fun getTodos(handler: (Request, Response, Result<List<Todo>, FuelError>) -> Unit) {
        Fuel.get("/" + "todos")
            .responseObject(handler)
    }

    fun postTodos(body: Todo, handler: (Request, Response, Result<Todo, FuelError>) -> Unit) {
        Fuel.post("/" + "todos")
            .body(Gson().toJson(body, Todo::class.java))
            .responseObject(handler)
    }

    fun putTodosById(capture_id: Int, body: Todo, handler: (Request, Response, Result<Unit, FuelError>) -> Unit) {
        Fuel.put("/" + "todos" + "/" + capture_id)
            .body(Gson().toJson(body, Todo::class.java))
            .responseObject(handler)
    }

    fun deleteTodosById(capture_id: Int, handler: (Request, Response, Result<Unit, FuelError>) -> Unit) {
        Fuel.delete("/" + "todos" + "/" + capture_id)
            .responseObject(handler)
    }

}