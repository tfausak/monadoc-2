# https://www.terraform.io/docs/configuration/terraform.html
terraform {
  required_version = "~> 0.12.12"

  # https://www.terraform.io/docs/backends/types/s3.html
  backend "s3" {
    bucket = "monadoc"
    key    = "monadoc.tfstate"
    region = "us-east-1"
  }
}

# https://www.terraform.io/docs/providers/aws/index.html
provider "aws" {
  version = "~> 2.33"
}

variable "commit" {
  type = string
}

# https://www.terraform.io/docs/providers/aws/r/api_gateway_rest_api.html
resource "aws_api_gateway_rest_api" "this" {
  name = "example-haskell-lambda-function-API"

  endpoint_configuration {
    types = ["REGIONAL"]
  }
}

# https://www.terraform.io/docs/providers/aws/r/api_gateway_resource.html
resource "aws_api_gateway_resource" "this" {
  parent_id   = aws_api_gateway_rest_api.this.root_resource_id
  path_part   = "{proxy+}"
  rest_api_id = aws_api_gateway_rest_api.this.id
}

# https://www.terraform.io/docs/providers/aws/r/api_gateway_method.html
resource "aws_api_gateway_method" "this" {
  authorization      = "NONE"
  http_method        = "ANY"
  request_parameters = { "method.request.path.proxy" = true }
  resource_id        = aws_api_gateway_resource.this.id
  rest_api_id        = aws_api_gateway_rest_api.this.id
}

# https://www.terraform.io/docs/providers/aws/r/lambda_function.html
resource "aws_lambda_function" "this" {
  function_name = "arn:aws:lambda:us-east-1:014479108335:function:example-haskell-lambda-function"
  handler       = "monadoc.main"
  publish       = true
  role          = "arn:aws:iam::014479108335:role/service-role/example-haskell-lambda-function-role-vrvm2inv"
  runtime       = "provided"
  s3_bucket     = "monadoc"
  s3_key        = "functions/${var.commit}.zip"
}

# https://www.terraform.io/docs/providers/aws/r/api_gateway_integration.html
resource "aws_api_gateway_integration" "this" {
  cache_key_parameters    = ["method.request.path.proxy"]
  content_handling        = "CONVERT_TO_TEXT"
  http_method             = aws_api_gateway_method.this.http_method
  integration_http_method = "POST"
  resource_id             = aws_api_gateway_resource.this.id
  rest_api_id             = aws_api_gateway_rest_api.this.id
  type                    = "AWS_PROXY"
  uri                     = aws_lambda_function.this.invoke_arn
}

# TODO: Import more resources into Terraform.
