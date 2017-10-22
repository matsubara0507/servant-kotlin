package com.github.matsubara0507

import com.github.kittinunf.fuel.Fuel
import com.github.kittinunf.fuel.core.FuelError
import com.github.kittinunf.fuel.core.FuelManager
import com.github.kittinunf.fuel.core.Request
import com.github.kittinunf.fuel.core.Response
import com.github.kittinunf.fuel.gson.responseObject
import com.github.kittinunf.result.Result
import com.google.gson.Gson

class ScoreAPI(private val baseURL: String) {

    init {
        FuelManager.instance.apply {
            basePath = baseURL
            baseHeaders = mapOf("Content-Type" to "application/json", "Device" to "Android")
        }
    }

    data class Score(val textLength: Int, val clearTime: Int, val swapCount: Int)

    fun getScores(handler: (Request, Response, Result<List<Score>, FuelError>) -> Unit) {
        Fuel.get("/" + "scores")
            .responseObject(handler)
    }

    fun postScores(body: Score, handler: (Request, Response, Result<Score, FuelError>) -> Unit) {
        Fuel.post("/" + "scores")
            .body(Gson().toJSON(body, Score::class.java))
            .responseObject(handler)
    }

}