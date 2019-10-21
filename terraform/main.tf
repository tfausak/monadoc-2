terraform {
  required_version = "~> 0.12.12"

  backend "s3" {
    bucket = "monadoc"
    key = "monadoc.tfstate"
    region = "us-east-1"
  }
}

provider "aws" {
  version = "~> 2.33"
}

variable "commit" {
  type = string
}

variable "publish" {
  default = false
  type = bool
}

resource "aws_lambda_function" "this" {
  function_name = "arn:aws:lambda:us-east-1:014479108335:function:example-haskell-lambda-function"
  handler = "hello.handler"
  publish = var.publish
  role = "arn:aws:iam::014479108335:role/service-role/example-haskell-lambda-function-role-vrvm2inv"
  runtime = "provided"
  s3_bucket = "monadoc"
  s3_key = "functions/${var.commit}.zip"
}
